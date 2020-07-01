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


module Types where


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
import           Data.List (concatMap, uncons, findIndex, dropWhile, takeWhile)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe, maybeToList)
import           Data.Proxy
import qualified Data.Set as S
import           Data.Text (Text, intercalate, split)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Time.Calendar
import qualified Data.UUID as U
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


newtype ThreadId = ThreadId { unThreadId :: Text }
  deriving (Eq, Ord, Show)


newtype CategoryId = CategoryId { unCategoryId :: Text }
  deriving (Eq, Ord, Show)


newtype CommentId = CommentId { unCommentId :: Text }
  deriving (Eq, Ord, Show)


newtype UserId = UserId { unUserId :: Text }
  deriving (Eq, Ord, Show)


newtype SessionId = SessionId { unSessionId :: Text }
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


instance Ord RelationLabel where
  compare (SymmL x) (SymmL y) = compare x y
  compare (AsymL x) (AsymL y) = compare x y
  compare (SymmL _) (AsymL _) = LT
  compare (AsymL _) (SymmL _) = GT


symmetricLabel :: Text -> RelationLabel
symmetricLabel = SymmL . SymmL'


asymmetricLabel :: Text -> Text -> RelationLabel
asymmetricLabel a p = AsymL (AsymL' a p)


unRelationLabel :: (Text -> a) -> (Text -> Text -> a) -> RelationLabel -> a
unRelationLabel f _ (SymmL (SymmL' a)) = f a
unRelationLabel _ g (AsymL (AsymL' a p)) = g a p


data Relation' (s :: Symmetry) = Rel (RelationLabel' s) (ThreadId, ThreadId)
deriving instance Eq (Relation' Symmetric)
deriving instance Eq (Relation' Asymmetric)
deriving instance Ord (Relation' Symmetric)
deriving instance Ord (Relation' Asymmetric)
deriving instance Show (Relation' Symmetric)
deriving instance Show (Relation' Asymmetric)


data Relation =
    Symm (Relation' Symmetric)
  | Asym (Relation' Asymmetric)
  deriving (Eq, Show)


instance Ord Relation where
  compare (Symm x) (Symm y) = compare x y
  compare (Asym x) (Asym y) = compare x y
  compare (Symm _) (Asym _) = LT
  compare (Asym _) (Symm _) = GT


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
  , categoryCreatedFrom :: Maybe CategoryId }
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


data Change = NewCategory CategoryId Text
            | NewThread ThreadId Text
            | NewComment ThreadId CommentId Text
            | NewEdit CommentId Text
            | AddThreadToCategory CategoryId ThreadId
            | RemoveThreadFromCategory CategoryId ThreadId
            | RetitleCategory CategoryId CategoryId Text
            | RetitleThread ThreadId ThreadId Text
            | RemoveComment ThreadId CommentId
            | TrashCategory CategoryId
            | UntrashCategory CategoryId
            | SplitThread ThreadId ThreadId ThreadId CommentId
            | AddCommentToThread ThreadId CommentId
            | AddCommentRangeToThread ThreadId CommentId CommentId ThreadId
            --                        from     start     end       to       (add at end)
            | MoveComment ThreadId CommentId Int -- index to move it to
            | MoveThread CategoryId ThreadId Int -- index to move it to
            | NewRelationLabel RelationLabel
            | DeleteRelationLabel RelationLabel
            | NewRelation Relation
            | DeleteRelation Relation
            | ComposedChanges [Change]
            deriving (Eq, Show)


data Session = Session
  { sessionId      :: SessionId
  , sessionUser    :: UserId
  , sessionCreated :: Day
  , sessionChanges :: [Change] }
  deriving (Eq, Show)


data Zettel = Zettel
  { categories :: M.Map CategoryId Category
  , threads :: M.Map ThreadId Thread
  , comments :: M.Map CommentId Comment
  , relations :: S.Set Relation
  , users :: M.Map UserId UserProfile
  , session :: Maybe Session }
  deriving (Eq, Show)


class ZettelEditor m where
  saveNewCategory :: CategoryId -> Text -> SessionId -> m ()
  saveNewThread :: ThreadId -> Text -> [CategoryId] -> SessionId -> m ()
  saveNewComment :: ThreadId -> CommentId -> Text -> SessionId -> m ()
  editComment :: CommentId -> Text -> SessionId -> m ()
  getDatabase :: SessionId -> m Zettel
  login :: UserId -> PasswordHash -> m (Maybe Session)


type API =      "api" :> "category" :> Capture "id" CategoryId :> QueryParam' '[Required] "title" Text
                :> QueryParam' '[Required] "session" SessionId :> Post '[JSON] ()
           :<|> "api" :> "thread"   :> Capture "id" ThreadId   :> QueryParam' '[Required] "title" Text
                :> QueryParam' '[Required] "categorization" [CategoryId]
                :> QueryParam' '[Required] "session" SessionId :> Post '[JSON] ()
           :<|> "api" :> "comment"  :> Capture "threadId" ThreadId :> Capture "id" CommentId
                :> ReqBody' '[Required] '[JSON] Text
                :> QueryParam' '[Required] "session" SessionId :> Post '[JSON] ()
           :<|> "api" :> "comment" :> Capture "id" CommentId
                :> ReqBody' '[Required] '[JSON] Text
                :> QueryParam' '[Required] "session" SessionId :> Post '[JSON] ()
           :<|> "api" :> QueryParam' '[Required] "session" SessionId :> Get '[JSON] Zettel
           :<|> "api" :> "login" :> Capture "id" UserId :> ReqBody' '[Required] '[OctetStream] PasswordHash
                 :> Post '[JSON] (Maybe Session)


data InitialV = InitialV { newCategoryTitle :: Text
                         , newThreadTitles :: M.Map CategoryId Text }
  deriving (Eq, Show)


data ThreadV = ThreadV { viewedThread :: Thread, newComment :: Text } deriving (Eq, Show)


data LoginV = LoginV { userIdField :: Text, passwordField :: Text } deriving (Eq, Show)


data View =
    InitialView InitialV
  | ThreadView ThreadV
  | LoginView LoginV
  deriving (Eq, Show)


type Model = (Zettel, View)


data Route = InitialRoute | ThreadRoute ThreadId | LoginRoute


type SPA = "app" :> Raw
           :<|> "app" :> "thread" :> Capture "id" ThreadId :> Raw
           :<|> "app" :> "login" :> Raw


routes :: SPA :>> Route
routes = InitialRoute :<|> ThreadRoute :<|> LoginRoute


router :: Monad m => Route -> Continuation m Model
router InitialRoute = pur $
  \(z, _) -> case session z of
    Just _  -> (z, InitialView (InitialV "" (M.fromList ((,"") <$> M.keys (categories z)))))
    Nothing -> (z, LoginView (LoginV "" ""))
router (ThreadRoute tid) = pur $
  (\(z, v) -> case (session z, M.lookup tid (threads z)) of
                (Just _, Just t) -> (z, ThreadView (ThreadV t ""))
                (Nothing, _) -> (z, LoginView (LoginV "" ""))
                _ -> (z, v))
router LoginRoute = pur $ \(z, _) -> (z, LoginView (LoginV "" ""))


type ModelCoproduct = Either (Either (Zettel, InitialV) (Zettel, ThreadV)) (Zettel, LoginV)


coproductToModel :: ModelCoproduct -> Model
coproductToModel (Left (Left (z, i))) = (z, InitialView i)
coproductToModel (Left (Right (z, t))) = (z, ThreadView t)
coproductToModel (Right (z, l)) = (z, LoginView l)


modelToCoproduct :: Model -> ModelCoproduct
modelToCoproduct (z, InitialView i) = Left (Left (z, i))
modelToCoproduct (z, ThreadView t) = Left (Right (z, t))
modelToCoproduct (z, LoginView l) = Right (z, l)


coproductIsoModel :: EndoIso ModelCoproduct Model
coproductIsoModel = piiso coproductToModel modelToCoproduct


emptyZettel :: Zettel
emptyZettel = Zettel mempty mempty mempty mempty mempty Nothing


whoAmI :: Zettel -> Maybe UserId
whoAmI = fmap sessionUser . session


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


invertChange :: Zettel -> Change -> Change
invertChange _ (NewCategory c _) = TrashCategory c
invertChange _ (NewComment t c _) = RemoveComment t c
invertChange z (NewEdit c x) = ComposedChanges . maybeToList
  $ NewEdit c . commentText <$> M.lookup c (comments z)
invertChange z (AddThreadToCategory c t) = ComposedChanges $
  do _ <- maybeToList $ M.lookup c (categories z)
     _ <- maybeToList $ M.lookup t (threads z)
     [RemoveThreadFromCategory c t]
invertChange z (RemoveThreadFromCategory c t) = ComposedChanges $
  do _ <- maybeToList $ M.lookup c (categories z)
     _ <- maybeToList $ M.lookup t (threads z)
     [AddThreadToCategory c t]
invertChange z (RetitleCategory f t x) = ComposedChanges $
  do _ <- maybeToList $ M.lookup f (categories z)
     [TrashCategory t, UntrashCategory f]
invertChange z (RetitleThread f t x) = ComposedChanges $
  do thread <- maybeToList (M.lookup f (threads z))
     c <- categorization thread
     [RemoveThreadFromCategory c t, AddThreadToCategory c f]
invertChange z (RemoveComment t c) = ComposedChanges $
  do comment <- maybeToList (M.lookup c (comments z))
     thread <- maybeToList (M.lookup t (threads z))
     i <- maybeToList $ findIndex (== c) (threadCommentIds thread)
     [AddCommentToThread t c, MoveComment t c i]
invertChange _ (TrashCategory c) = UntrashCategory c
invertChange _ (UntrashCategory c) = TrashCategory c
invertChange z (SplitThread t f s c) = ComposedChanges $
  do thread <- maybeToList (M.lookup t (threads z))
     c <- categorization thread
     [RemoveThreadFromCategory c f, RemoveThreadFromCategory c s, AddThreadToCategory c t]
invertChange _ (AddCommentToThread t c) = RemoveComment t c
invertChange z (AddCommentRangeToThread f s e t) = ComposedChanges $
  do thread <- maybeToList (M.lookup f (threads z))
     c <- takeWhile (not . (== e)) . dropWhile (not . (== s)) $ threadCommentIds thread
     [RemoveComment t c]
invertChange z (MoveComment t c _) = ComposedChanges $
  do thread <- maybeToList (M.lookup t (threads z))
     i <- maybeToList . findIndex (== c) $ threadCommentIds thread
     return $ MoveComment t c i
invertChange z (MoveThread c t _) = ComposedChanges $
  do cat <- maybeToList (M.lookup c (categories z))
     i <- maybeToList . findIndex (== t) $ categoryThreadIds cat
     return $ MoveThread c t i
invertChange _ (NewRelationLabel l) = DeleteRelationLabel l
invertChange _ (DeleteRelationLabel l) = NewRelationLabel l
invertChange _ (NewRelation r) = DeleteRelation r
invertChange z (ComposedChanges cs) = ComposedChanges $ invertChange z <$> reverse cs


applyChange :: UserId -> Day -> Change -> Zettel -> Zettel
applyChange _ _ (NewCategory i t) z = z { categories = M.insert i (Category t i [] Nothing) (categories z) }
applyChange u d (NewThread i t) z = z { threads = M.insert i (Thread i t u d [] [] Nothing) (threads z) }
applyChange u d (NewComment it ic t) z = fromMaybe z $ do
  th <- M.lookup it (threads z)
  return $ z { comments = M.insert ic (Comment ic u d [Edit d t]) (comments z)
             , threads = M.insert it th { threadCommentIds = threadCommentIds th ++ [ic] } (threads z) }
applyChange u d (NewEdit i t) z = fromMaybe z $ do
  c <- M.lookup i (comments z)
  guard (u == commentAuthor c)
  return $ z { comments = M.insert i (c { commentEdits = (Edit d t) : commentEdits c }) (comments z) }
applyChange _ _ (AddThreadToCategory ic it) z = fromMaybe z $ do
  c <- M.lookup ic (categories z)
  t <- M.lookup it (threads z)
  return $ z { categories = M.insert ic (c { categoryThreadIds = categoryThreadIds c ++ [it] }) (categories z)
             , threads = M.insert it (t { categorization = categorization t ++ [ic] }) (threads z) }
applyChange _ _ (RemoveThreadFromCategory ic it) z = fromMaybe z $ do
  c <- M.lookup ic (categories z)
  t <- M.lookup it (threads z)
  return $ z { categories = M.insert ic (c { categoryThreadIds = filter (/= it) (categoryThreadIds c) }) (categories z)
             , threads = M.insert it (t { categorization = filter (/= ic) (categorization t) }) (threads z) }


initialViewModel :: Zettel -> InitialV
initialViewModel z = InitialV "" (M.fromList $ (,"") <$> M.keys (categories z))


initialModel :: Route -> Zettel -> Model
initialModel InitialRoute z = (z, InitialView (initialViewModel z))
initialModel (ThreadRoute tid) z = case M.lookup tid (threads z) of
  Just t  -> (z, ThreadView (ThreadV t ""))
  Nothing -> (z, InitialView (initialViewModel z))
initialModel LoginRoute z = (z, LoginView (LoginV "" ""))


setUserId :: (Zettel, LoginV) -> Text -> (Zettel, LoginV)
setUserId (z, LoginV _ p) u = (z, LoginV u p)


setPassword :: (Zettel, LoginV) -> Text -> (Zettel, LoginV)
setPassword (z, LoginV u _) p = (z, LoginV u p)


hash :: Text -> JSM PasswordHash
hash t = PasswordHash <$> (jsg1 ("sha256" :: Text) (val t) >>= valToText)


handleLogin :: MonadJSM m => ZettelEditor m => Continuation m (Zettel, LoginV)
handleLogin = Continuation . (id,) $ \(z, LoginV u p) -> do
  h   <- liftJSM (hash p)
  return . Continuation . (second (const (LoginV "" "")),) $ \_ -> do
    res <- login (UserId u) h
    case res of
      Just s ->
        return . Continuation . (first (\z' -> z' { session = Just s }),)
        $ \(z',v) -> do setStorage "session" (sessionId s)
                        navigate @SPA InitialRoute
                        z'' <- getDatabase (sessionId s)
                        return . Continuation . (first (const z''),)
                          . const . return . causes $ navigate @SPA InitialRoute
      Nothing -> return (pur id)


reload :: Monad m => ZettelEditor m => Continuation m (Zettel, InitialV)
reload = Continuation . (id,) $ \(z,_) ->
  case session z of
    Just s -> do
      z' <- getDatabase (sessionId s)
      return . pur . const $ (z', initialViewModel z')
    Nothing -> return (pur id)


addCategory :: MonadUnliftIO m => ZettelEditor m => Continuation m (Zettel, InitialV)
addCategory = Continuation . (id,) $ \(z,i) -> do
  newId <- CategoryId . U.toText <$> liftIO randomIO
  case session z of
    Just s -> do
      let f (z',i') =
            let c = Category (newCategoryTitle i) newId [] Nothing
            in ( z' { categories = M.insert (categoryId c) c (categories z') }
               , i' { newCategoryTitle = ""
                    , newThreadTitles = M.insert (categoryId c) "" (newThreadTitles i') } )
      return . Continuation . (f,) . const . return . causes . void . forkIO $
        saveNewCategory newId (newCategoryTitle i) (sessionId s)


setNewCategoryTitle :: (Zettel, InitialV) -> Text -> (Zettel, InitialV)
setNewCategoryTitle (z, i) t = (z, i { newCategoryTitle = t })


addThread :: MonadJSM m => MonadUnliftIO m => ZettelEditor m => Category -> Continuation m (Zettel, InitialV)
addThread cat = Continuation . (id,) $ \(z,i) ->
  case (M.lookup (categoryId cat) (newThreadTitles i), session z, whoAmI z) of
    (Just t, Just s, Just u) -> do
      newId <- ThreadId . U.toText <$> liftIO randomIO
      today <- getToday
      let f model@(z',i') =
            let newThread = Thread newId t u today [] [categoryId cat] Nothing
                z'' = z' { threads = M.insert newId newThread (threads z')
                         , categories = M.insert (categoryId cat)
                           (cat { categoryThreadIds = categoryThreadIds cat ++ [newId] })
                           (categories z') }
                i'' = i' { newThreadTitles = M.insert (categoryId cat) "" (newThreadTitles i') }
            in (z'', i'')
      return . Continuation . (f,) . const . return . causes . void . forkIO
        $ saveNewThread newId t [categoryId cat] (sessionId s)
    _ -> return (pur id)


setNewThreadTitle :: (Zettel, InitialV) -> Category -> Text -> (Zettel, InitialV)
setNewThreadTitle model cat t =
  second (\i -> i { newThreadTitles = M.insert (categoryId cat) t (newThreadTitles i) })
  model


getNewThreadTitle :: (Zettel, InitialV) -> Category -> Text
getNewThreadTitle (_, i) cat = fromMaybe "" $ M.lookup (categoryId cat) (newThreadTitles i)


addComment :: MonadJSM m => MonadUnliftIO m => ZettelEditor m => Continuation m (Zettel, ThreadV)
addComment = Continuation . (id,) $ \(z, ThreadV t txt) -> do
  today <- getToday
  cid   <- CommentId . U.toText <$> liftIO randomIO
  case (session z, whoAmI z) of
    (Just s, Just u) ->
      let f (z', v) =
            let t'  = fromMaybe t (M.lookup (threadId t) (threads z'))
                t'' = t' { threadCommentIds = threadCommentIds t' ++ [cid] }
                z'' = z' { threads = M.insert (threadId t) t'' (threads z')
                         , comments = M.insert cid (Comment cid u today [Edit today txt])
                                      (comments z') }
                v'  = v { viewedThread = t'', newComment = "" }
            in (z'', v')
      in return . Continuation . (f,) . const . return . causes . void . forkIO
         $ saveNewComment (threadId t) cid txt (sessionId s)
    _ -> return (pur id)


setNewComment :: (Zettel, ThreadV) -> Text -> (Zettel, ThreadV)
setNewComment (z, v) t = (z, v { newComment = t })


getToday :: MonadJSM m => m Day
getToday = do
  date  <- liftJSM $ eval ("new Date()" :: Text) >>= makeObject
  day   <- round <$> liftJSM (((date # ("getDay" :: Text) :: [JSVal] -> JSM JSVal) $ []) >>= valToNumber)
  month <- round <$> liftJSM (((date # ("getMonth" :: Text) :: [JSVal] -> JSM JSVal) $ []) >>= valToNumber)
  year  <- round <$> liftJSM (((date # ("getYear" :: Text) :: [JSVal] -> JSM JSVal) $ []) >>= valToNumber)
  return (fromGregorian year month day)


instance ToHttpApiData a => ToHttpApiData [a] where
  toUrlPiece xs = intercalate "," $ toUrlPiece <$> xs


instance FromHttpApiData a => FromHttpApiData [a] where
  parseUrlPiece t = either (const (Right [])) Right . sequence
                    $ parseUrlPiece <$> filter (/= "") (split (== ',') t)


instance FromHttpApiData ThreadId where
  parseUrlPiece = return . ThreadId


instance ToHttpApiData ThreadId where
  toUrlPiece = unThreadId


instance FromJSON ThreadId where
  parseJSON (String s) = pure (ThreadId s)
  parseJSON x = fail "ThreadId: Expected String"


instance ToJSON ThreadId where
  toJSON = String . unThreadId


instance FromHttpApiData CategoryId where
  parseUrlPiece = return . CategoryId


instance ToHttpApiData CategoryId where
  toUrlPiece = unCategoryId


instance FromJSON CategoryId where
  parseJSON (String s) = pure (CategoryId s)
  parseJSON _ = fail "CategoryId: Expected String"


instance ToJSON CategoryId where
  toJSON = String . unCategoryId


instance FromHttpApiData CommentId where
  parseUrlPiece = return . CommentId


instance ToHttpApiData CommentId where
  toUrlPiece = unCommentId


instance FromJSON CommentId where
  parseJSON (String s) = pure (CommentId s)
  parseJSON _ = fail "CommentId: Expected String"


instance ToJSON CommentId where
  toJSON = String . unCommentId


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
  parseJSON (String s) = pure (SessionId s)
  parseJSON x = fail "SessionId: Expected String"


instance ToJSON SessionId where
  toJSON = String . unSessionId


instance FromHttpApiData SessionId where
  parseUrlPiece = return . SessionId


instance ToHttpApiData SessionId where
  toUrlPiece = unSessionId


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
    return (Category t i ts f)


instance ToJSON Category where
  toJSON c = object [ "title" .= categoryTitle c
                    , "id" .= categoryId c
                    , "threads" .= categoryThreadIds c
                    , "from" .= categoryCreatedFrom c ]


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
      Right (Rel label (ThreadId s, ThreadId p))
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


instance FromJSON Change where
  parseJSON x = (withObject "Change" $ \o ->
        parseNewCategory o <|> parseNewThread o <|> parseNewComment o
    <|> parseNewEdit o <|> parseAddThreadToCategory o <|> parseRemoveThreadFromCategory o
    <|> parseRetitleCategory o <|> parseRetitleThread o <|> parseRemoveComment o
    <|> parseTrashCategory o <|> parseUntrashCategory o <|> parseSplitThread o
    <|> parseAddCommentToThread o <|> parseAddCommentRangeToThread o
    <|> parseMoveComment o <|> parseMoveThread o <|> parseNewRelationLabel o
    <|> parseDeleteRelationLabel o <|> parseNewRelation o <|> parseDeleteRelation o) x
    <|> (ComposedChanges <$> parseJSON x)

    where
      parseNewCategory o = do
        i <- o .: "categoryId"
        t <- o .: "title"
        return (NewCategory i t)

      parseNewThread o = do
        i <- o .: "threadId"
        t <- o .: "title"
        return (NewThread i t)

      parseNewComment o = do
        t <- o .: "threadId"
        i <- o .: "commentId"
        x <- o .: "text"
        return (NewComment i t x)

      parseNewEdit o = do
        i <- o .: "commentId"
        x <- o .: "text"
        return (NewEdit i x)

      parseAddThreadToCategory o = do
        c <- o .: "toCategoryId"
        t <- o .: "addThreadId"
        return (AddThreadToCategory c t)

      parseRemoveThreadFromCategory o = do
        c <- o .: "fromCategoryId"
        t <- o .: "removeThreadId"
        return (RemoveThreadFromCategory c t)

      parseRetitleCategory o = do
        f <- o .: "fromCategoryId"
        t <- o .: "toCategoryId"
        r <- o .: "retitle"
        return (RetitleCategory f t r)

      parseRetitleThread o = do
        f <- o .: "fromThreadId"
        t <- o .: "toThreadId"
        r <- o .: "retitle"
        return (RetitleThread f t r)

      parseRemoveComment o = do
        t <- o .: "fromThreadId"
        c <- o .: "removeCommentId"
        return (RemoveComment t c)

      parseTrashCategory o = do
        i <- o .: "trashCategoryId"
        return (TrashCategory i)

      parseUntrashCategory o = do
        i <- o .: "nonTrashCategoryId"
        return (UntrashCategory i)

      parseSplitThread o = do
        t <- o .: "splitThreadId"
        f <- o .: "intoFirstPartThreadId"
        s <- o .: "intoSecondPartThreadId"
        c <- o .: "atCommentId"
        return (SplitThread t f s c)

      parseAddCommentToThread o = do
        t <- o .: "toThreadId"
        c <- o .: "addCommentId"
        return (AddCommentToThread t c)

      parseAddCommentRangeToThread o = do
        i <- o .: "fromThreadId"
        f <- o .: "addFromCommentId"
        t <- o .: "toCommentId"
        j <- o .: "toThreadId"
        return (AddCommentRangeToThread i f t j)

      parseMoveComment o = do
        t <- o .: "inThreadId"
        c <- o .: "moveCommentId"
        i <- o .: "toIndex"
        return (MoveComment t c i)

      parseMoveThread o = do
        c <- o .: "inCategoryId"
        t <- o .: "moveThreadId"
        i <- o .: "toIndex"
        return (MoveThread c t i)

      parseNewRelationLabel o = do
        l <- o .: "relationLabel"
        return (NewRelationLabel l)

      parseDeleteRelationLabel o = do
        l <- o .: "deleteRelationLabel"
        return (DeleteRelationLabel l)

      parseNewRelation o = do
        r <- o .: "relation"
        return (NewRelation r)

      parseDeleteRelation o = do
        r <- o .: "deleteRelation"
        return (DeleteRelation r)


instance ToJSON Change where
  toJSON (NewCategory i t) = object [ "categoryId" .= i, "title" .= t ]
  toJSON (NewThread i t) = object [ "threadId" .= i, "title" .= t]
  toJSON (NewComment t i x) = object [ "threadId" .= t, "commentId" .= i, "text" .= x]
  toJSON (NewEdit i x) = object [ "commentId" .= i, "text" .= x ]
  toJSON (AddThreadToCategory c t) = object [ "toCategoryId" .= c, "addThreadId" .= t ]
  toJSON (RemoveThreadFromCategory c t) = object [ "fromCategoryId" .= c, "removeThreadId" .= t ]
  toJSON (RetitleCategory f t r) = object [ "fromCategoryId" .= f, "toCategoryId" .= t, "retitle" .= r ]
  toJSON (RetitleThread f t r) = object [ "fromThreadId" .= f, "toThreadId" .= t, "retitle" .= r ]
  toJSON (RemoveComment t c) = object [ "fromThreadId" .= t, "removeCommentId" .= c ]
  toJSON (TrashCategory i) = object [ "trashCategoryId" .= i ]
  toJSON (UntrashCategory i) = object [ "nonTrashCategoryId" .= i ]
  toJSON (SplitThread t f s c) = object [ "splitThreadId" .= t
                                        , "intoFirstPartThreadId" .= f
                                        , "intoSecondPartThreadId" .= s
                                        , "atCommentId" .= c ]
  toJSON (AddCommentToThread t c) = object [ "toThreadId" .= t, "addCommentId" .= c ]
  toJSON (AddCommentRangeToThread i f t j) = object [ "fromThreadId" .= i
                                                    , "addFromCommentId" .= f
                                                    , "toCommentId" .= t
                                                    , "toThreadId" .= j ]
  toJSON (MoveComment t c i) = object [ "inThreadId" .= t
                                      , "moveCommentId" .= c
                                      , "toIndex" .= i ]
  toJSON (NewRelationLabel l) = object [ "relationLabel" .= l ]
  toJSON (DeleteRelationLabel l) = object [ "deleteRelationLabel" .= l ]
  toJSON (NewRelation r) = object [ "relation" .= r ]
  toJSON (DeleteRelation r) = object [ "deleteRelation" .= r ]


instance FromJSON Session where
  parseJSON = withObject "Session" $ \o -> do
    i <- o .: "id"
    u <- o .: "user"
    c <- o .: "created"
    h <- o .: "changes"
    return (Session i u c h)


instance ToJSON Session where
  toJSON s = object
    [ "id" .= sessionId s
    , "user" .= sessionUser s
    , "created" .= sessionCreated s
    , "changes" .= sessionChanges s ]


mapBy :: Ord k => (v -> k) -> [v] -> M.Map k v
mapBy toKey xs = M.fromList $ (\x -> (toKey x, x)) <$> xs


instance FromJSON Zettel where
  parseJSON = withObject "Zettel" $ \o -> do
    cs <- o .: "categories"
    ts <- o .: "threads"
    xs <- o .: "comments"
    rs <- o .: "relations"
    us <- o .: "users"
    s  <- o .: "session"
    return $ Zettel
      { categories = mapBy categoryId cs
      , threads = mapBy threadId ts
      , comments = mapBy commentId xs
      , relations = S.fromList rs
      , users = mapBy userId us
      , session = s }


instance ToJSON Zettel where
  toJSON z = object
             [ "categories" .= M.elems (categories z)
             , "threads" .= M.elems (threads z)
             , "comments" .= M.elems (comments z)
             , "relations" .= S.toList (relations z)
             , "users" .= M.elems (users z)
             , "session" .= session z ]


instance (Monad m, ZettelEditor m) => ZettelEditor (ParDiffT model m) where
  saveNewCategory i t s = lift $ saveNewCategory i t s
  saveNewThread i t cs s = lift $ saveNewThread i t cs s
  saveNewComment t i x s = lift $ saveNewComment t i x s
  editComment i t s = lift $ editComment i t s
  getDatabase s = lift $ getDatabase s
  login u p = lift $ login u p


(saveNewCategoryM :<|> saveNewThreadM :<|> saveNewCommentM :<|> editCommentM :<|> getDatabaseM :<|> loginM) = client (Proxy @API)


newtype App a = App { runApp :: JSM a }


instance Functor App where
  fmap f (App m) = App (fmap f m)


instance Applicative App where
  pure x = App (pure x)
  (App f) <*> (App x) = App (f <*> x)


instance Monad App where
  return x = App (return x)
  (App m) >>= f = App (m >>= runApp . f)


instance MonadIO App where
  liftIO = App . liftIO


instance MonadThrow App where
  throwM e = App (throwM e)


#ifndef ghcjs_HOST_OS
instance MonadJSM App where
  liftJSM' = App
#endif


instance MonadUnliftIO App where
  askUnliftIO = do
    c <- askJSM
    return $ UnliftIO $ \(App m) -> runJSaddle @IO c m


instance ZettelEditor App where
  saveNewCategory i t s = runXHR App $ saveNewCategoryM i t s
  saveNewThread i t  cs s = runXHR App $ saveNewThreadM i t cs s
  saveNewComment t i x s = runXHR App $ saveNewCommentM t i x s
  editComment c x s = runXHR App $ editCommentM c x s
  getDatabase s = runXHR App $ getDatabaseM s
  login u p = runXHR App $ loginM u p


instance Routed SPA Route where
  redirect = \case
    InitialRoute -> Redirect (Proxy @("app" :> Raw)) id
    ThreadRoute tid -> Redirect (Proxy @("app" :> "thread" :> Capture "id" ThreadId :> Raw)) ($ tid)
