{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Types where


import           Control.Arrow (second)
import           Control.Monad (void)
import           Control.Monad.Catch (MonadThrow (..))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.PseudoInverseCategory (EndoIso, piiso)
import           Control.ShpadoinkleContinuation
import           Data.Aeson
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Proxy
import           Data.Text (Text, intercalate, split)
import           Data.Text.Encoding (encodeUtf8)
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


newtype UserId = UserId { unUserId :: Text }
  deriving (Eq, Ord, Show)


newtype SessionId = SessionId { unSessionId :: Text }
  deriving (Eq, Read, Show)


newtype PasswordHash = PasswordHash { unPasswordHash :: Text }
  deriving (Eq, Read, Show)


data Link = Link { linkTo :: ThreadId, linkDescription :: Text }
  deriving (Eq, Show)


data Comment = Comment
  { commentAuthor :: UserId
  , commentCreated :: Day
  , commentText :: Text }
  deriving (Eq, Show)


data Category = Category
  { categoryTitle :: Text
  , categoryId :: CategoryId
  , categoryThreadIds :: [ThreadId] }
  deriving (Eq, Show)


data Thread = Thread
  { threadId :: ThreadId
  , threadTitle :: Text
  , threadAuthor :: UserId
  , threadCreated :: Day
  , comments :: [Comment]
  , links :: [Link]
  , categorization :: [CategoryId] }
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
  deriving (Eq, Show)


data Zettel = Zettel
  { categories :: M.Map CategoryId Category
  , threads :: M.Map ThreadId Thread
  , users :: M.Map UserId UserProfile
  , session :: Maybe Session }
  deriving (Eq, Show)


class ZettelEditor m where
  saveNewCategory :: CategoryId -> Text -> SessionId -> m ()
  saveNewThread :: ThreadId -> Text -> [Link] -> [CategoryId] -> SessionId -> m ()
  saveNewComment :: ThreadId -> Text -> SessionId -> m ()
  getDatabase :: SessionId -> m Zettel
  login :: UserId -> PasswordHash -> m (Maybe Session)


type API =      "api" :> "category" :> Capture "id" CategoryId :> QueryParam' '[Required] "title" Text
                :> QueryParam' '[Required] "session" SessionId :> Post '[JSON] ()
           :<|> "api" :> "thread"   :> Capture "id" ThreadId   :> QueryParam' '[Required] "title" Text
                :> QueryParam' '[Required] "links" [Link] :> QueryParam' '[Required] "categorization" [CategoryId]
                :> QueryParam' '[Required] "session" SessionId :> Post '[JSON] ()
           :<|> "api" :> "comment"  :> Capture "id" ThreadId  :> ReqBody' '[Required] '[JSON] Text
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
emptyZettel = Zettel mempty mempty mempty Nothing


whoAmI :: Zettel -> Maybe UserId
whoAmI = fmap sessionUser . session


categoryThreads :: Zettel -> Category -> [Thread]
categoryThreads z c = catMaybes $ flip M.lookup (threads z) <$> (categoryThreadIds c)


categoryIdTitle :: Zettel -> CategoryId -> Maybe Text
categoryIdTitle z i = categoryTitle <$> M.lookup i (categories z)


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
  res <- login (UserId u) h
  case res of
    Just s -> 
      return . Continuation . ((\(z',v) -> (z' { session = Just s }, v)),)
        . const . return . causes $ do
          setStorage "session" (sessionId s)
          navigate @SPA InitialRoute
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
            let c = Category (newCategoryTitle i) newId []
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
            let newThread = Thread newId t u today [] [] [categoryId cat]
                z'' = z' { threads = M.insert newId newThread (threads z')
                         , categories = M.insert (categoryId cat)
                           (cat { categoryThreadIds = categoryThreadIds cat ++ [newId] })
                           (categories z') }
                i'' = i' { newThreadTitles = M.insert (categoryId cat) "" (newThreadTitles i') }
            in (z'', i'')
      return . Continuation . (f,) . const . return . causes . void . forkIO
        $ saveNewThread newId t [] [categoryId cat] (sessionId s)
    _ -> return (pur id)


setNewThreadTitle :: (Zettel, InitialV) -> Category -> Text -> (Zettel, InitialV)
setNewThreadTitle model cat t =
  second (\i -> i { newThreadTitles = M.insert (categoryId cat) t (newThreadTitles i) })
  model


getNewThreadTitle :: (Zettel, InitialV) -> Category -> Text
getNewThreadTitle (_, i) cat = fromMaybe "" $ M.lookup (categoryId cat) (newThreadTitles i)


addComment :: MonadJSM m => MonadUnliftIO m => ZettelEditor m => Continuation m (Zettel, ThreadV)
addComment = Continuation . (id,) $ \(z,ThreadV t c) -> do
  today <- getToday
  case (session z, whoAmI z) of
    (Just s, Just u) ->
      let f (z', v) =
            let t'  = fromMaybe t (M.lookup (threadId t) (threads z'))
                t'' = t' { comments = comments t' ++ [Comment u today c] }
                z'' = z' { threads = M.insert (threadId t) t'' (threads z') }
                v'  = v { viewedThread = t'', newComment = "" }
            in (z'', v')
      in return . Continuation . (f,) . const . return . causes . void . forkIO
         $ saveNewComment (threadId t) c (sessionId s)
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
  parseJSON x = fail "CategoryId: Expected String"


instance ToJSON CategoryId where
  toJSON = String . unCategoryId


instance FromJSON UserId where
  parseJSON (String s) = pure (UserId s)
  parseJSON x = fail "UserId: Expected String"


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


instance FromHttpApiData PasswordHash where
  parseUrlPiece = return . PasswordHash


instance ToHttpApiData PasswordHash where
  toUrlPiece = unPasswordHash


instance MimeRender OctetStream PasswordHash where
  mimeRender _ = fromStrict . encodeUtf8 . unPasswordHash


instance FromHttpApiData Link where
  parseUrlPiece t = case split (== '-') t of
    [x,y] -> Link <$> parseUrlPiece x <*> parseUrlPiece y
    _ -> fail "Link"


instance ToHttpApiData Link where
  toUrlPiece (Link t d) = d <> "-" <> toUrlPiece t


instance FromJSON Link where
  parseJSON = withObject "Link" $ \o -> do
    t <- o .: "to"
    d <- o .: "description"
    return (Link t d)


instance ToJSON Link where
  toJSON l = object [ "to" .= linkTo l, "description" .= linkDescription l ]


instance FromJSON Category where
  parseJSON = withObject "Category" $ \o -> do
    t  <- o .: "title"
    i  <- o .: "id"
    ts <- o .: "threads"
    return (Category t i ts)


instance ToJSON Category where
  toJSON c = object [ "title" .= categoryTitle c, "id" .= categoryId c, "threads" .= categoryThreadIds c ]


instance FromJSON Thread where
  parseJSON = withObject "Thread" $ \o -> do
    i  <- o .: "id"
    t  <- o .: "title"
    a  <- o .: "author"
    c  <- o .: "created"
    cs <- o .: "comments"
    ls <- o .: "links"
    cg <- o .: "categorization"
    return (Thread i t a c cs ls cg)


instance ToJSON Thread where
  toJSON t = object
    [ "id" .= threadId t
    , "title" .= threadTitle t
    , "author" .= threadAuthor t
    , "created" .= threadCreated t
    , "comments" .= comments t
    , "links" .= links t
    , "categorization" .= categorization t ]


instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    a <- o .: "author"
    c <- o .: "created"
    t <- o .: "text"
    return (Comment a c t)


instance ToJSON Comment where
  toJSON c = object
    [ "author" .= commentAuthor c
    , "created" .= commentCreated c
    , "text" .= commentText c ]


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
    , "created" .= sessionCreated s ]


mapBy :: Ord k => (v -> k) -> [v] -> M.Map k v
mapBy toKey xs = M.fromList $ (\x -> (toKey x, x)) <$> xs


instance FromJSON Zettel where
  parseJSON = withObject "Zettel" $ \o -> do
    cs <- o .: "categories"
    ts <- o .: "threads"
    us <- o .: "users"
    s  <- o .: "session"
    return $ Zettel (mapBy categoryId cs) (mapBy threadId ts) (mapBy userId us) s


instance ToJSON Zettel where
  toJSON z = object [ "categories" .= M.elems (categories z), "threads" .= M.elems (threads z) ]


instance (Monad m, ZettelEditor m) => ZettelEditor (ParDiffT model m) where
  saveNewCategory i t s = lift $ saveNewCategory i t s
  saveNewThread i t ls cs s = lift $ saveNewThread i t ls cs s
  saveNewComment i t s = lift $ saveNewComment i t s
  getDatabase s = lift $ getDatabase s
  login u p = lift $ login u p


(saveNewCategoryM :<|> saveNewThreadM :<|> saveNewCommentM :<|> getDatabaseM :<|> loginM) = client (Proxy @API)


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
  saveNewThread i t ls cs s = runXHR App $ saveNewThreadM i t ls cs s
  saveNewComment i t s = runXHR App $ saveNewCommentM i t s
  getDatabase s = runXHR App $ getDatabaseM s
  login u p = runXHR App $ loginM u p


instance Routed SPA Route where
  redirect = \case
    InitialRoute -> Redirect (Proxy @("app" :> Raw)) id
    ThreadRoute tid -> Redirect (Proxy @("app" :> "thread" :> Capture "id" ThreadId :> Raw)) ($ tid)
