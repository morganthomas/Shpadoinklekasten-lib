{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
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
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Proxy
import           Data.Text (Text, intercalate, split)
import qualified Data.UUID as U
import           GHC.Generics
import           Language.Javascript.JSaddle (MonadJSM (..), JSM, askJSM, runJSaddle)
import           Servant.API hiding (Link)
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Router
import           Shpadoinkle.Router.Client (client, runXHR)
import           System.Random (randomIO)
import           UnliftIO
import           UnliftIO.Concurrent (forkIO)


newtype ThreadId = ThreadId { unThreadId :: Text }
  deriving (Eq, Ord, Show)


newtype CategoryId = CategoryId { unCategoryId :: Text }
  deriving (Eq, Ord, Show)


data Link = Link { linkTo :: ThreadId, linkDescription :: Text }
  deriving (Eq, Show)


data Category = Category
  { categoryTitle :: Text
  , categoryId :: CategoryId
  , categoryThreadIds :: [ThreadId] }
  deriving (Eq, Show)


data Thread = Thread
  { threadId :: ThreadId
  , threadTitle :: Text
  , comments :: [Text]
  , links :: [Link]
  , categorization :: [CategoryId] }
  deriving (Eq, Show)


data Zettel = Zettel
  { categories :: M.Map CategoryId Category
  , threads :: M.Map ThreadId Thread }
  deriving (Eq, Show)


class ZettelEditor m where
  saveNewCategory :: CategoryId -> Text -> m ()
  saveNewThread :: ThreadId -> Text -> [Link] -> [CategoryId] -> m ()
  saveNewComment :: ThreadId -> Text -> m ()
  getDatabase :: m Zettel


type API =      "api" :> "category" :> Capture "id" CategoryId :> QueryParam' '[Required] "title" Text :> Post '[JSON] ()
           :<|> "api" :> "thread"   :> Capture "id" ThreadId   :> QueryParam' '[Required] "title" Text :> QueryParam' '[Required] "links" [Link] :> QueryParam' '[Required] "categorization" [CategoryId] :> Post '[JSON] ()
           :<|> "api" :> "comment"  :> Capture "id" ThreadId   :> ReqBody' '[Required] '[JSON] Text :> Post '[JSON] ()
           :<|> "api" :> Get '[JSON] Zettel


data InitialV = InitialV { newCategoryTitle :: Text
                         , newThreadTitles :: M.Map CategoryId Text }
  deriving (Eq, Show)


data ThreadV = ThreadV { viewedThread :: Thread, newComment :: Text } deriving (Eq, Show)


data View =
    InitialView InitialV
  | ThreadView ThreadV
  deriving (Eq, Show)


type Model = (Zettel, View)


data Route = InitialRoute | ThreadRoute ThreadId


type SPA = "app" :> Raw
           :<|> "app" :> "thread" :> Capture "id" ThreadId :> Raw


routes :: SPA :>> Route
routes = InitialRoute :<|> ThreadRoute


router :: Monad m => Route -> Continuation m Model
router InitialRoute = pur $
  \(z, _) -> (z, InitialView (InitialV "" (M.fromList ((,"") <$> M.keys (categories z)))))
router (ThreadRoute tid) = pur (\(z, v) -> case M.lookup tid (threads z) of
                                   Just t -> (z, ThreadView (ThreadV t ""))
                                   Nothing -> (z, v))


type ModelCoproduct = Either (Zettel, InitialV) (Zettel, ThreadV)


coproductToModel :: ModelCoproduct -> Model
coproductToModel (Left (z, i)) = (z, InitialView i)
coproductToModel (Right (z, t)) = (z, ThreadView t)


modelToCoproduct :: Model -> ModelCoproduct
modelToCoproduct (z, InitialView i) = Left (z, i)
modelToCoproduct (z, ThreadView t) = Right (z, t)


coproductIsoModel :: EndoIso ModelCoproduct Model
coproductIsoModel = piiso coproductToModel modelToCoproduct


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


reload :: Monad m => ZettelEditor m => Continuation m (Zettel, InitialV)
reload = impur $ do
  z <- getDatabase
  return . const $ (z, initialViewModel z)


addCategory :: MonadUnliftIO m => ZettelEditor m => Continuation m (Zettel, InitialV)
addCategory = Continuation . (id,) $ \(z,i) -> do
  newId <- CategoryId . U.toText <$> liftIO randomIO
  saveNewCategory newId (newCategoryTitle i)
  let f (z',i') =
        let c = Category (newCategoryTitle i) newId []
        in ( z' { categories = M.insert (categoryId c) c (categories z') }
           , i' { newCategoryTitle = ""
                , newThreadTitles = M.insert (categoryId c) "" (newThreadTitles i') } )
  return . Continuation . (f,) . const . return . causes . void . forkIO $
    saveNewCategory newId (newCategoryTitle i)


setNewCategoryTitle :: (Zettel, InitialV) -> Text -> (Zettel, InitialV)
setNewCategoryTitle (z, i) t = (z, i { newCategoryTitle = t })


addThread :: MonadUnliftIO m => ZettelEditor m => Category -> Continuation m (Zettel, InitialV)
addThread cat = Continuation . (id,) $ \(z,i) ->
  case M.lookup (categoryId cat) (newThreadTitles i) of
    Just t -> do
      newId <- ThreadId . U.toText <$> liftIO randomIO
      let f model@(z',i') =
            let newThread = Thread newId t [] [] [categoryId cat]
                z'' = z' { threads = M.insert newId newThread (threads z')
                         , categories = M.insert (categoryId cat)
                           (cat { categoryThreadIds = categoryThreadIds cat ++ [newId] })
                           (categories z') }
                i'' = i' { newThreadTitles = M.insert (categoryId cat) "" (newThreadTitles i') }
            in (z'', i'')
      return . Continuation . (f,) . const . return . causes . void . forkIO
        $ saveNewThread newId t [] [categoryId cat]
    Nothing -> return (pur id)


setNewThreadTitle :: (Zettel, InitialV) -> Category -> Text -> (Zettel, InitialV)
setNewThreadTitle model cat t =
  second (\i -> i { newThreadTitles = M.insert (categoryId cat) t (newThreadTitles i) })
  model


getNewThreadTitle :: (Zettel, InitialV) -> Category -> Text
getNewThreadTitle (_, i) cat = fromMaybe "" $ M.lookup (categoryId cat) (newThreadTitles i)


addComment :: MonadUnliftIO m => ZettelEditor m => Continuation m (Zettel, ThreadV)
addComment = Continuation . (id,) $ \(_,ThreadV t c) ->
  let f (z, v) =
          let t'  = fromMaybe t (M.lookup (threadId t) (threads z))
              t'' = t' { comments = comments t' ++ [c] }
              z'  = z { threads = M.insert (threadId t) t'' (threads z) }
              v'  = v { viewedThread = t'', newComment = "" }
          in (z', v')
  in return . Continuation . (f,) . const . return . causes . void . forkIO
       $ saveNewComment (threadId t) c


setNewComment :: (Zettel, ThreadV) -> Text -> (Zettel, ThreadV)
setNewComment (z, v) t = (z, v { newComment = t })


instance ToHttpApiData a => ToHttpApiData [a] where
  toUrlPiece xs = intercalate "," $ toUrlPiece <$> xs


instance FromHttpApiData a => FromHttpApiData [a] where
  parseUrlPiece t = either (const (Right [])) Right . fmap concat . sequence
                    $ parseUrlPiece <$> split (== ',') t


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
    cs <- o .: "comments"
    ls <- o .: "links"
    cg <- o .: "categorization"
    return (Thread i t cs ls cg)


instance ToJSON Thread where
  toJSON t = object
    [ "id" .= threadId t
    , "title" .= threadTitle t
    , "comments" .= comments t
    , "links" .= links t
    , "categorization" .= categorization t ]


mapBy :: Ord k => (v -> k) -> [v] -> M.Map k v
mapBy toKey xs = M.fromList $ (\x -> (toKey x, x)) <$> xs


instance FromJSON Zettel where
  parseJSON = withObject "Zettel" $ \o -> do
    cs <- o .: "categories"
    ts <- o .: "threads"
    return $ Zettel (mapBy categoryId cs) (mapBy threadId ts)


instance ToJSON Zettel where
  toJSON z = object [ "categories" .= M.elems (categories z), "threads" .= M.elems (threads z) ]


instance (Monad m, ZettelEditor m) => ZettelEditor (ParDiffT model m) where
  saveNewCategory i t = lift $ saveNewCategory i t
  saveNewThread i t ls cs = lift $ saveNewThread i t ls cs
  saveNewComment i t = lift $ saveNewComment i t
  getDatabase = lift getDatabase


(saveNewCategoryM :<|> saveNewThreadM :<|> saveNewCommentM :<|> getDatabaseM) = client (Proxy @API)


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
  saveNewCategory i t = runXHR App $ saveNewCategoryM i t
  saveNewThread i t ls cs = runXHR App $ saveNewThreadM i t ls cs
  saveNewComment i t = runXHR App $ saveNewCommentM i t
  getDatabase = runXHR App getDatabaseM


instance Routed SPA Route where
  redirect = \case
    InitialRoute -> Redirect (Proxy @("app" :> Raw)) id
    ThreadRoute tid -> Redirect (Proxy @("app" :> "thread" :> Capture "id" ThreadId :> Raw)) ($ tid)
