{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}


module Types.Model where


import           Control.Applicative ((<|>))
import           Control.Arrow (first, second)
import           Control.Monad (void, guard)
import           Control.Monad.Catch (MonadThrow (..))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.PseudoInverseCategory (EndoIso, piiso)
import           Control.ShpadoinkleContinuation
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Aeson
import qualified Data.Bson as B
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Either.Extra (eitherToMaybe)
import           Data.Function ((&))
import           Data.List (concatMap, uncons, findIndex, dropWhile, takeWhile)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe, maybeToList, isNothing)
import           Data.Proxy
import qualified Data.Set as S
import           Data.Text (Text, intercalate, split, reverse)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Time.Calendar
import           Data.Tuple.Extra (uncurry3)
import qualified Data.UUID as U
import           Data.UUID.Next (nextUUID)
import           GHC.Generics
import           Language.Javascript.JSaddle (MonadJSM (..), JSM, JSVal, liftJSM, askJSM, runJSaddle, valToNumber, valToText, eval, (#), makeObject, toJSString, jsg1, val)
import           Servant.API hiding (Link)
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html.LocalStorage
import           Shpadoinkle.Router
import           Shpadoinkle.Router.Client
import           System.Random (randomIO)
import           UnliftIO
import           UnliftIO.Concurrent (forkIO)


data Route = InitialRoute | ThreadRoute ThreadId | LoginRoute


newtype ThreadId = ThreadId { unThreadId :: U.UUID }
  deriving (Eq, Ord, Show)


newtype CategoryId = CategoryId { unCategoryId :: U.UUID }
  deriving (Eq, Ord, Show)


newtype CommentId = CommentId { unCommentId :: U.UUID }
  deriving (Eq, Ord, Show)


newtype UserId = UserId { unUserId :: Text }
  deriving (Eq, Ord, Read, Show)


newtype SessionId = SessionId { unSessionId :: U.UUID }
  deriving (Eq, Read, Show)


newtype PasswordHash = PasswordHash { unPasswordHash :: Text }
  deriving (Eq, Read, Show)


data Symmetry = Symmetric | Asymmetric


data family RelationLabel' :: Symmetry -> *
data instance RelationLabel' Symmetric = SymmL' Text
  deriving (Eq, Ord, Show)
data instance RelationLabel' Asymmetric = AsymL' Text Text
  deriving (Eq, Ord, Show)


data RelationLabel =
    SymmL (RelationLabel' Symmetric)
  | AsymL (RelationLabel' Asymmetric)
  deriving (Eq, Show)


data Relation' (s :: Symmetry) = Rel (RelationLabel' s) (ThreadId, ThreadId)


data Relation =
    Symm (Relation' Symmetric)
  | Asym (Relation' Asymmetric)
  deriving (Eq, Show)


data Edit = Edit
  { editCreated :: Day
  , editText :: Text }
  deriving (Eq, Show)


data Comment = Comment
  { commentId :: CommentId
  , commentAuthor :: UserId
  , commentCreated :: Day
  , commentEdits :: [Edit] }
  deriving (Eq, Show)


data Category = Category
  { categoryTitle :: Text
  , categoryId :: CategoryId
  , categoryThreadIds :: [ThreadId]
  , categoryCreatedFrom :: Maybe CategoryId
  , categoryIsTrash :: Bool }
  deriving (Eq, Show)


data Thread = Thread
  { threadId :: ThreadId
  , threadTitle :: Text
  , threadAuthor :: UserId
  , threadCreated :: Day
  , threadCommentIds :: [CommentId]
  , categorization :: [CategoryId]
  , threadCreatedFrom :: Maybe ThreadId }
  deriving (Eq, Show)


data UserProfile = UserProfile
  { userId :: UserId
  , userFullName :: Text
  , userEmail :: Text
  , userCreated :: Day }
  deriving (Eq, Show)


data Session = Session
  { sessionId      :: SessionId
  , sessionUser    :: UserId
  , sessionCreated :: Day }
  deriving (Eq, Read, Show)


data Zettel = Zettel
  { categories :: M.Map CategoryId Category
  , categoryOrdering :: [CategoryId]
  , threads :: M.Map ThreadId Thread
  , trashcan :: S.Set ThreadId
  , comments :: M.Map CommentId Comment
  , relationLabels :: S.Set RelationLabel
  , relations :: S.Set Relation
  , users :: M.Map UserId UserProfile
  , session :: Maybe Session }
  deriving (Eq, Show)


emptyZettel :: Zettel
emptyZettel = Zettel mempty [] mempty mempty mempty mempty mempty mempty Nothing


whoAmI :: Zettel -> Maybe UserId
whoAmI = fmap sessionUser . session


emptyLabel :: RelationLabel
emptyLabel = asymmetricLabel "" ""


symmetricLabel :: Text -> RelationLabel
symmetricLabel = SymmL . SymmL'


asymmetricLabel :: Text -> Text -> RelationLabel
asymmetricLabel a p = AsymL (AsymL' a p)


unRelationLabel :: (Text -> a) -> (Text -> Text -> a) -> RelationLabel -> a
unRelationLabel f _ (SymmL (SymmL' a)) = f a
unRelationLabel _ g (AsymL (AsymL' a p)) = g a p


categoryThreads :: Zettel -> Category -> [Thread]
categoryThreads z c = catMaybes $ flip M.lookup (threads z) <$> categoryThreadIds c


categoryIdTitle :: Zettel -> CategoryId -> Maybe Text
categoryIdTitle z i = categoryTitle <$> M.lookup i (categories z)


threadComments :: Zettel -> Thread -> [Comment]
threadComments z t = catMaybes $ flip M.lookup (comments z) <$> threadCommentIds t


commentText :: Comment -> Text
commentText c = fromMaybe "" $ editText . fst <$> uncons (commentEdits c)

commentEdited :: Comment -> Bool
commentEdited c = case commentEdits c of
                    (_:_:_) -> True
                    _       -> False


commentLastEdited :: Comment -> Day
commentLastEdited c = fromMaybe (fromGregorian 0 0 0) $ editCreated . fst <$> uncons (commentEdits c)


hash :: Text -> JSM PasswordHash
#ifdef ghcjs_HOST_OS
hash t = PasswordHash <$> (jsg1 ("sha256" :: Text) (val t) >>= valToText)
#else
hash = return . PasswordHash . decodeUtf8 . Base16.encode . SHA256.hash . encodeUtf8
#endif


nextThreadId :: Zettel -> ThreadId -> ThreadId
nextThreadId z (ThreadId i) =
  let i' = ThreadId (nextUUID i) in
  case M.lookup i' (threads z) of
    Just _ -> nextThreadId z i'
    Nothing -> i'


data Gregorian = Gregorian { gregYear :: Integer, gregMonth :: Int, gregDay :: Int }
  deriving (Eq, Show)


gregToDay :: Gregorian -> Maybe Day
gregToDay g = fromGregorianValid (gregYear g) (gregMonth g) (gregDay g)


dayToGreg :: Day -> Gregorian
dayToGreg = uncurry3 Gregorian . toGregorian


instance B.Val Gregorian where
  val g = B.Doc
          [ "year" B.:= B.val (gregYear g)
          , "month" B.:= B.val (gregMonth g)
          , "day" B.:= B.val (gregDay g) ]
  cast' (B.Doc d) = Gregorian <$> (d B.!? "year" >>= B.cast')
                              <*> (d B.!? "month" >>= B.cast')
                              <*> (d B.!? "day" >>= B.cast')
  cast' _ = Nothing


instance Ord RelationLabel where
  compare (SymmL x) (SymmL y) = compare x y
  compare (AsymL x) (AsymL y) = compare x y
  compare (SymmL _) (AsymL _) = LT
  compare (AsymL _) (SymmL _) = GT


instance Ord Relation where
  compare (Symm x) (Symm y) = compare x y
  compare (Asym x) (Asym y) = compare x y
  compare (Symm _) (Asym _) = LT
  compare (Asym _) (Symm _) = GT


deriving instance Typeable (Relation' Symmetric)
deriving instance Typeable (Relation' Asymmetric)
deriving instance Eq (Relation' Symmetric)
deriving instance Eq (Relation' Asymmetric)
deriving instance Ord (Relation' Symmetric)
deriving instance Ord (Relation' Asymmetric)
deriving instance Show (Relation' Symmetric)
deriving instance Show (Relation' Asymmetric)


instance ToHttpApiData a => ToHttpApiData [a] where
  toUrlPiece xs = intercalate "," $ toUrlPiece <$> xs


instance FromHttpApiData a => FromHttpApiData [a] where
  parseUrlPiece t = either (const (Right [])) Right . sequence
                    $ parseUrlPiece <$> filter (/= "") (split (== ',') t)


instance FromHttpApiData ThreadId where
  parseUrlPiece = maybe (Left "ThreadId: expected a UUID") (Right . ThreadId) . U.fromText


instance ToHttpApiData ThreadId where
  toUrlPiece = U.toText . unThreadId


instance FromJSON ThreadId where
  parseJSON (String s) = maybe (fail "ThreadId: expected a UUID") (pure . ThreadId) (U.fromText s)
  parseJSON x = fail "ThreadId: expected String"


instance ToJSON ThreadId where
  toJSON = String . U.toText . unThreadId


instance B.Val ThreadId where
  val = B.String . U.toText . unThreadId
  cast' v = ThreadId <$> (B.cast' v >>= U.fromText)


instance FromHttpApiData CategoryId where
  parseUrlPiece = maybe (Left "CategoryId: expected a UUID") (Right . CategoryId) . U.fromText


instance ToHttpApiData CategoryId where
  toUrlPiece = U.toText . unCategoryId


instance FromJSON CategoryId where
  parseJSON (String s) = maybe (fail "CategoryId: expected a UUID") (pure . CategoryId) (U.fromText s)
  parseJSON _ = fail "CategoryId: expected String"


instance ToJSON CategoryId where
  toJSON = String . U.toText . unCategoryId


instance B.Val CategoryId where
  val = B.String . U.toText . unCategoryId
  cast' v = CategoryId <$> (B.cast' v >>= U.fromText)


instance FromHttpApiData CommentId where
  parseUrlPiece = maybe (Left "CommentId: expected a UUID") (Right . CommentId) . U.fromText


instance ToHttpApiData CommentId where
  toUrlPiece = U.toText . unCommentId


instance FromJSON CommentId where
  parseJSON (String s) = maybe (fail "CommentId: expected a UUID") (pure . CommentId) (U.fromText s)
  parseJSON _ = fail "CommentId: expected String"


instance ToJSON CommentId where
  toJSON = String . U.toText . unCommentId


instance B.Val CommentId where
  val = B.String . U.toText . unCommentId
  cast' v = CommentId <$> (B.cast' v >>= U.fromText)


instance FromJSON UserId where
  parseJSON (String s) = pure (UserId s)
  parseJSON _ = fail "UserId: Expected String"


instance ToJSON UserId where
  toJSON = String . unUserId


instance B.Val UserId where
  val = B.String . unUserId
  cast' v = UserId <$> B.cast' v


instance FromHttpApiData UserId where
  parseUrlPiece = return . UserId


instance ToHttpApiData UserId where
  toUrlPiece = unUserId


instance FromJSON SessionId where
  parseJSON (String s) = maybe (fail "SessionId: expected a UUID") (pure . SessionId) (U.fromText s)
  parseJSON x = fail "SessionId: expected String"


instance ToJSON SessionId where
  toJSON = String . U.toText . unSessionId


instance B.Val SessionId where
  val = B.String . U.toText . unSessionId
  cast' v = SessionId <$> (B.cast' v >>= U.fromText)


instance FromHttpApiData SessionId where
  parseUrlPiece = maybe (Left "SessionId: expected a UUID") (Right . SessionId) . U.fromText


instance ToHttpApiData SessionId where
  toUrlPiece = U.toText . unSessionId


instance MimeRender OctetStream PasswordHash where
  mimeRender _ = fromStrict . encodeUtf8 . unPasswordHash


instance MimeUnrender OctetStream PasswordHash where
  mimeUnrender _ = Right . PasswordHash . decodeUtf8 . toStrict


instance B.Val PasswordHash where
  val = B.String . unPasswordHash
  cast' v = PasswordHash <$> B.cast' v


instance FromJSON Category where
  parseJSON = withObject "Category" $ \o -> do
    t  <- o .: "title"
    i  <- o .: "id"
    ts <- o .: "threads"
    f  <- o .: "from"
    h  <- o .: "isTrash"
    return (Category t i ts f h)


instance ToJSON Category where
  toJSON c = object [ "title" .= categoryTitle c
                    , "id" .= categoryId c
                    , "threads" .= categoryThreadIds c
                    , "from" .= categoryCreatedFrom c
                    , "isTrash" .= categoryIsTrash c ]


instance B.Val Category where
  val x = B.Doc
          [ "title" B.:= B.val (categoryTitle x)
          , "id" B.:= B.val (categoryId x)
          , "threads" B.:= B.val (categoryThreadIds x)
          , "from" B.:= B.val (categoryCreatedFrom x)
          , "isTrash" B.:= B.val (categoryIsTrash x) ]
  cast' (B.Doc d) = do
    t  <- B.lookup "title" d
    i  <- B.lookup "id" d
    ts <- B.lookup "threads" d
    f  <- B.lookup "from" d
    iT <- B.lookup "isTrash" d
    return (Category t i ts f iT)
  cast' _ = Nothing


instance FromJSON Thread where
  parseJSON = withObject "Thread" $ \o -> do
    i  <- o .: "id"
    t  <- o .: "title"
    a  <- o .: "author"
    c  <- o .: "created"
    cs <- o .: "comments"
    cg <- o .: "categorization"
    f  <- o .: "from"
    return (Thread i t a c cs cg f)


instance ToJSON Thread where
  toJSON t = object
    [ "id" .= threadId t
    , "title" .= threadTitle t
    , "author" .= threadAuthor t
    , "created" .= threadCreated t
    , "comments" .= threadCommentIds t
    , "categorization" .= categorization t
    , "from" .= threadCreatedFrom t ]


instance B.Val Thread where
  val t = B.Doc
          [ "id" B.:= B.val (threadId t)
          , "title" B.:= B.val (threadTitle t)
          , "author" B.:= B.val (threadAuthor t)
          , "created" B.:= B.val (dayToGreg (threadCreated t))
          , "comments" B.:= B.val (threadCommentIds t)
          , "categorization" B.:= B.val (categorization t)
          , "from" B.:= B.val (threadCreatedFrom t) ]
  cast' (B.Doc d) = Thread <$> (d B.!? "id" >>= B.cast')
                           <*> (d B.!? "title" >>= B.cast')
                           <*> (d B.!? "author" >>= B.cast')
                           <*> (d B.!? "created" >>= B.cast' >>= gregToDay)
                           <*> (d B.!? "comments" >>= B.cast')
                           <*> (d B.!? "categorization" >>= B.cast')
                           <*> (d B.!? "from" >>= B.cast')
  cast' _ = Nothing


instance FromJSON Edit where
  parseJSON = withObject "Edit" $ \o -> do
    c <- o .: "created"
    t <- o .: "text"
    return (Edit c t)


instance ToJSON Edit where
  toJSON e = object [ "created" .= editCreated e, "text" .= editText e ]


instance B.Val Edit where
  val e = B.Doc
          [ "created" B.:= B.val (dayToGreg (editCreated e))
          , "text" B.:= B.val (editText e) ]
  cast' (B.Doc d) = Edit <$> (d B.!? "created" >>= B.cast' >>= gregToDay)
                         <*> (d B.!? "text" >>= B.cast')


instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    i  <- o .: "id"
    a  <- o .: "author"
    c  <- o .: "created"
    es <- o .: "edits"
    return (Comment i a c es)


instance ToJSON Comment where
  toJSON c = object
    [ "id" .= commentId c
    , "author" .= commentAuthor c
    , "created" .= commentCreated c
    , "edits" .= commentEdits c ]


instance B.Val Comment where
  val c = B.Doc
          [ "id" B.:= B.val (commentId c)
          , "created" B.:= B.val (dayToGreg (commentCreated c))
          , "author" B.:= B.val (commentAuthor c)
          , "edits" B.:= B.val (commentEdits c) ]
  cast' (B.Doc d) = Comment <$> (d B.!? "id" >>= B.cast')
                            <*> (d B.!? "author" >>= B.cast')
                            <*> (d B.!? "created" >>= B.cast' >>= gregToDay)
                            <*> (d B.!? "edits" >>= B.cast')
  cast' _ = Nothing


instance FromJSON UserProfile where
  parseJSON = withObject "UserProfile" $ \o -> do
    i <- o .: "id"
    n <- o .: "fullName"
    e <- o .: "email"
    c <- o .: "created"
    return (UserProfile i n e c)


instance ToJSON UserProfile where
  toJSON u = object
    [ "id" .= userId u
    , "fullName" .= userFullName u
    , "email" .= userEmail u
    , "created" .= userCreated u ]


instance B.Val UserProfile where
  val u = B.Doc
        [ "id" B.:= B.val (userId u)
        , "fullName" B.:= B.val (userFullName u)
        , "email" B.:= B.val (userEmail u)
        , "created" B.:= B.val (dayToGreg (userCreated u)) ]
  cast' (B.Doc d) = UserProfile <$> (d B.!? "id" >>= B.cast')
                                <*> (d B.!? "fullName" >>= B.cast')
                                <*> (d B.!? "email" >>= B.cast')
                                <*> (d B.!? "created" >>= B.cast' >>= gregToDay)
  cast' _ = Nothing


--instance FromJSON Symmetry where
--  parseJSON (String "symm") = pure Symmetric
--  parseJSON (String "asym") = pure Asymmetric


instance FromJSON (RelationLabel' Symmetric) where
  parseJSON = withObject "RelationLabel' Symmetric" $ \o -> do
    a <- o .: "activeVoice"
    return (SymmL' a)


instance ToJSON (RelationLabel' Symmetric) where
  toJSON (SymmL' a) = object [ "activeVoice" .= a ]


instance B.Val (RelationLabel' Symmetric) where
  val (SymmL' a) = B.Doc [ "activeVoice" B.:= B.String a ]
  cast' (B.Doc d) = SymmL' <$> (d B.!? "activeVoice" >>= B.cast')
  cast' _ = Nothing


instance ToHttpApiData (RelationLabel' Symmetric) where
  toUrlPiece (SymmL' a) = a


instance FromHttpApiData (RelationLabel' Symmetric) where
  parseUrlPiece = Right . SymmL'


instance FromJSON (RelationLabel' Asymmetric) where
  parseJSON = withObject "RelationLabel' Asymmetric" $ \o -> do
    a <- o .: "activeVoice"
    p <- o .: "passiveVoice"
    return (AsymL' a p)


instance ToJSON (RelationLabel' Asymmetric) where
  toJSON (AsymL' a p) = object [ "activeVoice" .= a, "passiveVoice" .= p ]


instance B.Val (RelationLabel' Asymmetric) where
  val (AsymL' a p) = B.Doc
                     [ "activeVoice" B.:= B.String a
                     , "passiveVoice" B.:= B.String p ]
  cast' (B.Doc d) = AsymL' <$> (d B.!? "activeVoice" >>= B.cast')
                           <*> (d B.!? "passiveVoice" >>= B.cast')


instance ToHttpApiData (RelationLabel' Asymmetric) where
  toUrlPiece (AsymL' a p) = a <> "," <> p


instance FromHttpApiData (RelationLabel' Asymmetric) where
  parseUrlPiece x = case split (== ',') x of
    (a:p:_) -> Right (AsymL' a p)
    _       -> Left ("Parse fail: RelationLabel' Asymmetric: " <> x)


instance FromJSON RelationLabel where
  parseJSON x = parseAsymmetricLabel x <|> parseSymmetricLabel x
    where parseSymmetricLabel o = SymmL <$> parseJSON x
          parseAsymmetricLabel x = AsymL <$> parseJSON x


instance ToJSON RelationLabel where
  toJSON = unRelationLabel (toJSON . SymmL') (\a p -> toJSON (AsymL' a p))


instance B.Val RelationLabel where
  val = unRelationLabel (B.val . SymmL') (\a p -> B.val (AsymL' a p))
  cast' v = (SymmL <$> B.cast' v) <|> (AsymL <$> B.cast' v)


instance FromHttpApiData RelationLabel where
  parseUrlPiece x = maybe (Left ("Parse fail: RelationLabel: " <> x)) Right $
    (AsymL <$> eitherToMaybe (parseUrlPiece x)) <|> (SymmL <$> eitherToMaybe (parseUrlPiece x))


instance ToHttpApiData RelationLabel where
  toUrlPiece (AsymL x) = toUrlPiece x
  toUrlPiece (SymmL x) = toUrlPiece x


instance FromJSON (RelationLabel' s) => FromJSON (Relation' s) where
  parseJSON = withObject "Relation'" $ \o -> do
    l <- o .: "label"
    s <- o .: "subject"
    p <- o .: "predicateObject"
    return (Rel l (s, p))


instance ToJSON (RelationLabel' s) => ToJSON (Relation' s) where
  toJSON (Rel l (s, p)) = object [ "label" .= l
                                 , "subject" .= s
                                 , "predicateObject" .= p ]


instance ( Typeable (Relation' s)
         , Eq (Relation' s)
         , Show (Relation' s)
         , B.Val (RelationLabel' s) )
         => B.Val (Relation' s) where
  val (Rel l (s, p)) = B.Doc
                       [ "label" B.:= B.val l
                       , "subject" B.:= B.val s
                       , "predicateObject" B.:= B.val p ]
  cast' (B.Doc d) = Rel <$> (d B.!? "label" >>= B.cast')
                <*> ((,) <$> (d B.!? "subject" >>= B.cast')
                         <*> (d B.!? "predicateObject" >>= B.cast'))
  cast' _ = Nothing


instance FromHttpApiData (RelationLabel' s) => FromHttpApiData (Relation' s) where
  parseUrlPiece x = case split (== ':') x of
    (l:s:p:_) -> do
      label <- parseUrlPiece l
      s' <- parseUrlPiece s
      p' <- parseUrlPiece p
      Right (Rel label (s', p'))
    _         -> Left ("Parse fail: FromHttpApiData (RelationLabel' s): " <> x)


instance ToHttpApiData (RelationLabel' s) => ToHttpApiData (Relation' s) where
  toUrlPiece (Rel l (s, p)) = toUrlPiece l <> ":" <> toUrlPiece s <> ":" <> toUrlPiece p


instance FromJSON Relation where
  parseJSON x = (Asym <$> parseJSON x) <|> (Symm <$> parseJSON x)


instance ToJSON Relation where
  toJSON (Symm r) = toJSON r
  toJSON (Asym r) = toJSON r


instance B.Val Relation where
  val (Symm r) = B.val r
  val (Asym r) = B.val r
  cast' v = (Asym <$> B.cast' v) <|> (Symm <$> B.cast' v)


instance FromHttpApiData Relation where
  parseUrlPiece x = maybe (Left ("Parse fail: Relation: " <> x)) Right $
    (Asym <$> eitherToMaybe (parseUrlPiece x)) <|> (Symm <$> (eitherToMaybe (parseUrlPiece x)))


instance ToHttpApiData Relation where
  toUrlPiece (Symm r) = toUrlPiece r
  toUrlPiece (Asym r) = toUrlPiece r


instance FromJSON Session where
  parseJSON = withObject "Session" $ \o -> do
    i <- o .: "id"
    u <- o .: "user"
    c <- o .: "created"
    return (Session i u c)


instance ToJSON Session where
  toJSON s = object
    [ "id" .= sessionId s
    , "user" .= sessionUser s
    , "created" .= sessionCreated s]


instance B.Val Session where
  val s = B.Doc
          [ "id" B.:= B.val (sessionId s)
          , "user" B.:= B.val (sessionUser s)
          , "created" B.:= B.val (dayToGreg (sessionCreated s)) ]
  cast' (B.Doc d) = Session
                <$> (d B.!? "id" >>= B.cast') 
                <*> (d B.!? "user" >>= B.cast')
                <*> (d B.!? "created" >>= B.cast' >>= gregToDay)


mapBy :: Ord k => (v -> k) -> [v] -> M.Map k v
mapBy toKey xs = M.fromList $ (\x -> (toKey x, x)) <$> xs


instance FromJSON Zettel where
  parseJSON = withObject "Zettel" $ \o -> do
    cs <- o .: "categories"
    co <- o .: "categoryOrdering"
    ts <- o .: "threads"
    tc <- o .: "trashcan"
    xs <- o .: "comments"
    ls <- o .: "relationLabels"
    rs <- o .: "relations"
    us <- o .: "users"
    s  <- o .: "session"
    return $ Zettel
      { categories = mapBy categoryId cs
      , categoryOrdering = co
      , threads = mapBy threadId ts
      , trashcan = S.fromList tc
      , comments = mapBy commentId xs
      , relationLabels = S.fromList ls
      , relations = S.fromList rs
      , users = mapBy userId us
      , session = s }


instance ToJSON Zettel where
  toJSON z = object
             [ "categories" .= M.elems (categories z)
             , "categoryOrdering" .= categoryOrdering z
             , "threads" .= M.elems (threads z)
             , "trashcan" .= S.toList (trashcan z)
             , "comments" .= M.elems (comments z)
             , "relationLabels" .= S.toList (relationLabels z)
             , "relations" .= S.toList (relations z)
             , "users" .= M.elems (users z)
             , "session" .= session z ]


instance B.Val Zettel where
  val z = B.Doc
          [ "categories" B.:= B.val (M.elems (categories z))
          , "categoryOrdering" B.:= B.val (categoryOrdering z)
          , "threads" B.:= B.val (M.elems (threads z))
          , "trashcan" B.:= B.val (S.toList (trashcan z))
          , "comments" B.:= B.val (M.elems (comments z))
          , "relationLabels" B.:= B.val (S.toList (relationLabels z))
          , "relations" B.:= B.val (S.toList (relations z))
          , "users" B.:= B.val (M.elems (users z))
          , "session" B.:= B.val (session z) ]
  cast' (B.Doc d) = Zettel
                <$> (mapBy categoryId <$> (d B.!? "categories" >>= B.cast'))
                <*> (d B.!? "categoryOrdering" >>= B.cast')
                <*> (mapBy threadId <$> (d B.!? "threads" >>= B.cast'))
                <*> (S.fromList <$> (d B.!? "trashcan" >>= B.cast'))
                <*> (mapBy commentId <$> (d B.!? "comments" >>= B.cast'))
                <*> (S.fromList <$> (d B.!? "relationLabels" >>= B.cast'))
                <*> (S.fromList <$> (d B.!? "relations" >>= B.cast'))
                <*> (mapBy userId <$> (d B.!? "users" >>= B.cast'))
                <*> (d B.!? "session" >>= B.cast')
  cast' _ = Nothing
