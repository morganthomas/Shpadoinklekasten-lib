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
import           Data.Aeson
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
import qualified Data.UUID as U
import           Data.UUID.Next (nextUUID)
import           GHC.Generics
import           Language.Javascript.JSaddle (MonadJSM (..), JSM, JSVal, liftJSM, askJSM, runJSaddle, valToNumber, valToText, eval, (#), makeObject, toJSString, jsg1, val)
import           Servant.API hiding (Link)
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html.LocalStorage
import           Shpadoinkle.Router
import           Shpadoinkle.Router.Client (client, runXHR)
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
  , threads :: M.Map ThreadId Thread
  , trashcan :: S.Set ThreadId
  , comments :: M.Map CommentId Comment
  , relationLabels :: S.Set RelationLabel
  , relations :: S.Set Relation
  , users :: M.Map UserId UserProfile
  , session :: Maybe Session }
  deriving (Eq, Show)


emptyZettel :: Zettel
emptyZettel = Zettel mempty mempty mempty mempty mempty mempty mempty Nothing


whoAmI :: Zettel -> Maybe UserId
whoAmI = fmap sessionUser . session


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
hash t = PasswordHash <$> (jsg1 ("sha256" :: Text) (val t) >>= valToText)


nextThreadId :: Zettel -> ThreadId -> ThreadId
nextThreadId z (ThreadId i) =
  let i' = ThreadId (nextUUID i) in
  case M.lookup i' (threads z) of
    Just _ -> nextThreadId z i'
    Nothing -> i'


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


instance FromHttpApiData CategoryId where
  parseUrlPiece = maybe (Left "CategoryId: expected a UUID") (Right . CategoryId) . U.fromText


instance ToHttpApiData CategoryId where
  toUrlPiece = U.toText . unCategoryId


instance FromJSON CategoryId where
  parseJSON (String s) = maybe (fail "CategoryId: expected a UUID") (pure . CategoryId) (U.fromText s)
  parseJSON _ = fail "CategoryId: expected String"


instance ToJSON CategoryId where
  toJSON = String . U.toText . unCategoryId


instance FromHttpApiData CommentId where
  parseUrlPiece = maybe (Left "CommentId: expected a UUID") (Right . CommentId) . U.fromText


instance ToHttpApiData CommentId where
  toUrlPiece = U.toText . unCommentId


instance FromJSON CommentId where
  parseJSON (String s) = maybe (fail "CommentId: expected a UUID") (pure . CommentId) (U.fromText s)
  parseJSON _ = fail "CommentId: expected String"


instance ToJSON CommentId where
  toJSON = String . U.toText . unCommentId


instance FromJSON UserId where
  parseJSON (String s) = pure (UserId s)
  parseJSON _ = fail "UserId: Expected String"


instance ToJSON UserId where
  toJSON = String . unUserId


instance FromHttpApiData UserId where
  parseUrlPiece = return . UserId


instance ToHttpApiData UserId where
  toUrlPiece = unUserId


instance FromJSON SessionId where
  parseJSON (String s) = maybe (fail "SessionId: expected a UUID") (pure . SessionId) (U.fromText s)
  parseJSON x = fail "SessionId: expected String"


instance ToJSON SessionId where
  toJSON = String . U.toText . unSessionId


instance FromHttpApiData SessionId where
  parseUrlPiece = maybe (Left "SessionId: expected a UUID") (Right . SessionId) . U.fromText


instance ToHttpApiData SessionId where
  toUrlPiece = U.toText . unSessionId


instance MimeRender OctetStream PasswordHash where
  mimeRender _ = fromStrict . encodeUtf8 . unPasswordHash


instance MimeUnrender OctetStream PasswordHash where
  mimeUnrender _ = Right . PasswordHash . decodeUtf8 . toStrict


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
    , "categorization" .= categorization t ]


instance FromJSON Edit where
  parseJSON = withObject "Edit" $ \o -> do
    c <- o .: "created"
    t <- o .: "text"
    return (Edit c t)


instance ToJSON Edit where
  toJSON e = object [ "created" .= editCreated e, "text" .= editText e ]


instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    i  <- o .: "id"
    a  <- o .: "author"
    c  <- o .: "created"
    es <- o .: "edits"
    return (Comment i a c es)


instance ToJSON Comment where
  toJSON c = object
    [ "author" .= commentAuthor c
    , "created" .= commentCreated c
    , "edits" .= commentEdits c ]


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


--instance FromJSON Symmetry where
--  parseJSON (String "symm") = pure Symmetric
--  parseJSON (String "asym") = pure Asymmetric


instance FromJSON (RelationLabel' Symmetric) where
  parseJSON = withObject "RelationLabel' Symmetric" $ \o -> do
    a <- o .: "activeVoice"
    return (SymmL' a)


instance ToJSON (RelationLabel' Symmetric) where
  toJSON (SymmL' a) = object [ "activeVoice" .= a ]


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
  toJSON = unRelationLabel unSymmetricLabel unAsymmetricLabel
    where unSymmetricLabel = object . (:[]) . ("activeVoice" .=)
          unAsymmetricLabel a p = object [ "activeVoice" .= a, "passiveVoice" .= p ]


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


mapBy :: Ord k => (v -> k) -> [v] -> M.Map k v
mapBy toKey xs = M.fromList $ (\x -> (toKey x, x)) <$> xs


instance FromJSON Zettel where
  parseJSON = withObject "Zettel" $ \o -> do
    cs <- o .: "categories"
    ts <- o .: "threads"
    tc <- o .: "trashcan"
    xs <- o .: "comments"
    ls <- o .: "relationLabels"
    rs <- o .: "relations"
    us <- o .: "users"
    s  <- o .: "session"
    return $ Zettel
      { categories = mapBy categoryId cs
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
             , "threads" .= M.elems (threads z)
             , "trashcan" .= S.toList (trashcan z)
             , "comments" .= M.elems (comments z)
             , "relationLabels" .= S.toList (relationLabels z)
             , "relations" .= S.toList (relations z)
             , "users" .= M.elems (users z)
             , "session" .= session z ]

