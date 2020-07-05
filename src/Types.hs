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


newtype ThreadId = ThreadId { unThreadId :: U.UUID }
  deriving (Eq, Ord, Show)


newtype CategoryId = CategoryId { unCategoryId :: U.UUID }
  deriving (Eq, Ord, Show)


newtype CommentId = CommentId { unCommentId :: U.UUID }
  deriving (Eq, Ord, Show)


newtype UserId = UserId { unUserId :: Text }
  deriving (Eq, Ord, Show)


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


symmetricLabel :: Text -> RelationLabel
symmetricLabel = SymmL . SymmL'


asymmetricLabel :: Text -> Text -> RelationLabel
asymmetricLabel a p = AsymL (AsymL' a p)


unRelationLabel :: (Text -> a) -> (Text -> Text -> a) -> RelationLabel -> a
unRelationLabel f _ (SymmL (SymmL' a)) = f a
unRelationLabel _ g (AsymL (AsymL' a p)) = g a p


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


data Change = NewCategory CategoryId Text
            | NewThread CategoryId ThreadId Text
            | NewComment ThreadId CommentId Text
            | NewEdit CommentId Text
            | AddThreadToCategory CategoryId ThreadId
            | RemoveThreadFromCategory CategoryId ThreadId
            | RetitleCategory CategoryId CategoryId Text
            | RetitleThread ThreadId ThreadId Text
            | RemoveComment ThreadId ThreadId CommentId
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
  , trashcan :: S.Set ThreadId
  , comments :: M.Map CommentId Comment
  , relationLabels :: S.Set RelationLabel
  , relations :: S.Set Relation
  , users :: M.Map UserId UserProfile
  , session :: Maybe Session }
  deriving (Eq, Show)


class ZettelEditor m where
  saveChange :: Change -> Session -> m ()
  getDatabase :: SessionId -> m Zettel
  login :: UserId -> PasswordHash -> m (Maybe Session)


type API =      "api" :> ReqBody' '[Required] '[JSON] (Change, SessionId) :> Post '[JSON] ()
           :<|> "api" :> QueryParam' '[Required] "session" SessionId :> Get '[JSON] Zettel
           :<|> "api" :> "login" :> Capture "id" UserId :> ReqBody' '[Required] '[OctetStream] PasswordHash
                 :> Post '[JSON] (Maybe Session)


data InitialV = InitialV { newCategoryTitle :: Text
                         , newThreadTitles :: M.Map CategoryId Text }
  deriving (Eq, Show)


data ThreadV = ThreadV { viewedThread :: Thread, commentField :: Text } deriving (Eq, Show)


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
emptyZettel = Zettel mempty mempty mempty mempty mempty mempty mempty Nothing


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


nextThreadId :: ThreadId -> ThreadId
nextThreadId (ThreadId i) = ThreadId (nextUUID i)


invertChange :: Zettel -> Change -> Change
invertChange _ (NewCategory c _) = TrashCategory c
invertChange _ (NewComment t c _) = RemoveComment t (nextThreadId t) c
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
     [ RemoveThreadFromCategory c t,
       AddThreadToCategory c f ]
invertChange z (RemoveComment f t c) = ComposedChanges $
  do comment <- maybeToList (M.lookup c (comments z))
     thread <- maybeToList (M.lookup f (threads z))
     i <- maybeToList $ findIndex (== c) (threadCommentIds thread)
     cat <- categorization thread
     [ RemoveThreadFromCategory cat t,
       AddThreadToCategory cat f ]
invertChange _ (TrashCategory c) = UntrashCategory c
invertChange _ (UntrashCategory c) = TrashCategory c
invertChange z (SplitThread t f s c) = ComposedChanges $
  do thread <- maybeToList (M.lookup t (threads z))
     c <- categorization thread
     [RemoveThreadFromCategory c f, RemoveThreadFromCategory c s, AddThreadToCategory c t]
invertChange _ (AddCommentToThread t c) = RemoveComment t (nextThreadId t) c
invertChange z (AddCommentRangeToThread f s e t) =
  let t' = nextThreadId t in ComposedChanges $
    do thread <- maybeToList (M.lookup f (threads z))
       c <- takeWhile (not . (== e)) . dropWhile (not . (== s)) $ threadCommentIds thread
       [RemoveComment t t' c]
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
invertChange z (ComposedChanges cs) = ComposedChanges $ invertChange z <$> Prelude.reverse cs


applyChange :: UserId -> Day -> Change -> Zettel -> Zettel
applyChange _ _ (NewCategory i t) z = fromMaybe z $ do
  guard . isNothing $ M.lookup i (categories z)
  return $ z { categories = M.insert i (Category t i [] Nothing False) (categories z) }
applyChange u d (NewThread ic it t) z = fromMaybe z $ do
  _ <- M.lookup ic (categories z)
  guard . isNothing $ M.lookup it (threads z)
  return $ z { threads = M.insert it (Thread it t u d [] [ic] Nothing) (threads z) }
applyChange u d (NewComment it ic t) z = fromMaybe z $ do
  guard . isNothing $ M.lookup ic (comments z)
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
             , threads = M.insert it (t { categorization = categorization t ++ [ic] }) (threads z)
             , trashcan = S.delete it (trashcan z) }
applyChange _ _ (RemoveThreadFromCategory ic it) z = fromMaybe z $ do
  c <- M.lookup ic (categories z)
  t <- M.lookup it (threads z)
  let cats' = filter (/= ic) (categorization t)
  return $ z { categories = M.insert ic (c { categoryThreadIds = filter (/= it) (categoryThreadIds c) }) (categories z)
             , threads = M.insert it (t { categorization = cats' }) (threads z)
             , trashcan = if null cats' then S.insert it (trashcan z) else trashcan z }
applyChange _ _ (RetitleCategory i i' t) z = fromMaybe z $ do
  c <- M.lookup i (categories z)
  guard . isNothing $ M.lookup i' (categories z)
  return $ z { categories = M.insert i (c { categoryIsTrash = True }) $
                            M.insert i' (c { categoryIsTrash = False
                                           , categoryId = i'
                                           , categoryTitle = t
                                           , categoryCreatedFrom = Just i })
                            (categories z) }
applyChange _ _ (RetitleThread i i' t) z = fromMaybe z $ do
  th <- M.lookup i (threads z)
  guard . isNothing $ M.lookup i' (threads z)
  return $ z { threads = M.insert i (th { categorization = [] }) $
                         M.insert i' (th { threadId = i'
                                         , threadTitle = t
                                         , threadCreatedFrom = Just i })
                         (threads z)
             , trashcan = S.insert i (trashcan z)
             , categories = replaceId <$> categories z }
  where replaceId c = c { categoryThreadIds = (\j -> if j == i then i' else j)
                                                <$> categoryThreadIds c }
applyChange _ _ (RemoveComment it it' ic) z = fromMaybe z $ do
  t <- M.lookup it (threads z)
  guard . isNothing $ M.lookup it' (threads z)
  return $ z { threads = M.insert it t { categorization = [] } $
                         M.insert it' t { threadId = it'
                                        , threadCommentIds = filter (/= ic) $ threadCommentIds t }
                         (threads z)
             , trashcan = S.insert it (trashcan z)
             , categories = replaceId <$> categories z }
  where replaceId c = c { categoryThreadIds = (\j -> if j == it then it' else j)
                                                <$> categoryThreadIds c }
applyChange _ _ (TrashCategory ic) z = fromMaybe z $ do
  c <- M.lookup ic (categories z)
  return $ z { categories = M.insert ic c { categoryIsTrash = True } (categories z) }
applyChange _ _ (UntrashCategory ic) z = fromMaybe z $ do
  c <- M.lookup ic (categories z)
  return $ z { categories = M.insert ic c { categoryIsTrash = False } (categories z) }
applyChange _ d (SplitThread it ia ib ic) z = fromMaybe z $ do
  t <- M.lookup it (threads z)
  c <- M.lookup ic (comments z)
  (csa, csb) <- splitOn ic (threadCommentIds t)
  return $ z { threads = M.insert it t { categorization = [] }
                       . M.insert ia (Thread
                                     { threadId = ia
                                     , threadTitle = threadTitle t
                                     , threadAuthor = threadAuthor t
                                     , threadCreated = d
                                     , threadCommentIds = csa
                                     , categorization = categorization t
                                     , threadCreatedFrom = Just it })
                       $ M.insert ib (Thread
                                     { threadId = ib
                                     , threadTitle = commentText c
                                     , threadAuthor = commentAuthor c
                                     , threadCreated = d
                                     , threadCommentIds = csb
                                     , categorization = categorization t
                                     , threadCreatedFrom = Just it })
                         (threads z)
             , trashcan = S.insert it (trashcan z)
             , categories = replaceIds <$> categories z }
  where replaceIds c = c { categoryThreadIds = interpolateAt it [ia, ib] (categoryThreadIds c) }
        interpolateAt :: Eq a => a -> [a] -> [a] -> [a]
        interpolateAt _ _  []     = []
        interpolateAt x ys (z:zs)
          | x == z    = ys ++ interpolateAt x ys zs
          | otherwise = z : interpolateAt x ys zs
applyChange _ _ (AddCommentToThread it ic) z = fromMaybe z $ do
  t <- M.lookup it (threads z)
  _ <- M.lookup ic (comments z)
  return $ z { threads = M.insert it t { threadCommentIds = threadCommentIds t ++ [ic] } (threads z) }
applyChange _ _ (AddCommentRangeToThread it is ie iu) z = fromMaybe z $ do
  t <- M.lookup it (threads z)
  u <- M.lookup iu (threads z)
  let cs = takeUntil (== ie) $ dropWhile (/= is) (threadCommentIds t)
  return $ z { threads = M.insert iu u { threadCommentIds = threadCommentIds u ++ cs } (threads z) }
  where takeUntil p []     = []
        takeUntil p (x:xs) = if p x then [x] else x : takeUntil p xs
applyChange _ _ (MoveComment it ic i) z = fromMaybe z $ do
  t <- M.lookup it (threads z)
  let cs = threadCommentIds t & filter (/= ic) & insertAt i ic
  return $ z { threads = M.insert it t { threadCommentIds = cs } (threads z) }
applyChange _ _ (MoveThread ic it i) z = fromMaybe z $ do
  c <- M.lookup ic (categories z)
  let ts = categoryThreadIds c & filter (/= it) & insertAt i it
  return $ z { categories = M.insert ic c { categoryThreadIds = ts } (categories z) }
applyChange _ _ (NewRelationLabel l) z = z { relationLabels = S.insert l (relationLabels z) }
applyChange _ _ (DeleteRelationLabel l) z = z { relationLabels = S.delete l (relationLabels z) }
applyChange _ _ (NewRelation r) z = z { relations = S.insert r (relations z) }
applyChange _ _ (DeleteRelation r) z = z { relations = S.delete r (relations z) }
applyChange u d (ComposedChanges cs) z = (foldl (.) id $ applyChange u d <$> cs) z


insertAt :: Int -> a -> [a] -> [a]
insertAt 0 y xs     = y:xs
insertAt n y []     = [y]
insertAt n y (x:xs) = x : insertAt (n-1) y xs


newCategory :: ZettelEditor m => CategoryId -> Text -> Session -> m ()
newCategory i t = saveChange (NewCategory i t)


newThread :: ZettelEditor m => CategoryId -> ThreadId -> Text -> Session -> m ()
newThread ic it t = saveChange (NewThread ic it t)


newComment :: ZettelEditor m => ThreadId -> CommentId -> Text -> Session -> m ()
newComment i c t = saveChange (NewComment i c t)


newEdit :: ZettelEditor m => CommentId -> Text -> Session -> m ()
newEdit i t = saveChange (NewEdit i t)


addThreadToCategory :: ZettelEditor m => CategoryId -> ThreadId -> Session -> m ()
addThreadToCategory c t = saveChange (AddThreadToCategory c t)


removeThreadFromCategory :: ZettelEditor m => CategoryId -> ThreadId -> Session -> m ()
removeThreadFromCategory c t = saveChange (RemoveThreadFromCategory c t)


retitleCategory :: ZettelEditor m => CategoryId -> CategoryId -> Text -> Session -> m ()
retitleCategory f t x = saveChange (RetitleCategory f t x)


retitleThread :: ZettelEditor m => ThreadId -> ThreadId -> Text -> Session -> m ()
retitleThread f t x = saveChange (RetitleThread f t x)


removeComment :: ZettelEditor m => ThreadId -> ThreadId -> CommentId -> Session -> m ()
removeComment f t c = saveChange (RemoveComment f t c)


trashCategory :: ZettelEditor m => CategoryId -> Session -> m ()
trashCategory c = saveChange (TrashCategory c)


untrashCategory :: ZettelEditor m => CategoryId -> Session -> m ()
untrashCategory c = saveChange (UntrashCategory c)


splitThread :: ZettelEditor m => ThreadId -> ThreadId -> ThreadId -> CommentId -> Session -> m ()
splitThread f a b c = saveChange (SplitThread f a b c)


addCommentToThread :: ZettelEditor m => ThreadId -> CommentId -> Session -> m ()
addCommentToThread t c = saveChange (AddCommentToThread t c)


addCommentRangeToThread :: ZettelEditor m => ThreadId -> CommentId -> CommentId -> ThreadId -> Session -> m ()
addCommentRangeToThread f s e t = saveChange (AddCommentRangeToThread f s e t)


moveComment :: ZettelEditor m => ThreadId -> CommentId -> Int -> Session -> m ()
moveComment t c i = saveChange (MoveComment t c i)


moveThread :: ZettelEditor m => CategoryId -> ThreadId -> Int -> Session -> m ()
moveThread c t i = saveChange (MoveThread c t i)


newRelationLabel :: ZettelEditor m => RelationLabel -> Session -> m ()
newRelationLabel l = saveChange (NewRelationLabel l)


deleteRelationLabel :: ZettelEditor m => RelationLabel -> Session -> m ()
deleteRelationLabel l = saveChange (DeleteRelationLabel l)


newRelation :: ZettelEditor m => Relation -> Session -> m ()
newRelation r = saveChange (NewRelation r)


deleteRelation :: ZettelEditor m => Relation -> Session -> m ()
deleteRelation r = saveChange (DeleteRelation r)


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


handleNewCategory :: MonadJSM m => MonadUnliftIO m => ZettelEditor m => Continuation m (Zettel, InitialV)
handleNewCategory = Continuation . (id,) $ \(z,i) -> do
  newId <- CategoryId <$> liftIO randomIO
  return $ maybe (pur id) (voidRunContinuationT . newCategory newId (newCategoryTitle i)) (session z)


setNewCategoryTitle :: (Zettel, InitialV) -> Text -> (Zettel, InitialV)
setNewCategoryTitle (z, i) t = (z, i { newCategoryTitle = t })


handleNewThread :: MonadJSM m => MonadUnliftIO m => ZettelEditor m
  => Category -> Continuation m (Zettel, InitialV)
handleNewThread cat = Continuation . (id,) $ \(z,i) ->
  case (M.lookup (categoryId cat) (newThreadTitles i), session z) of
    (Just t, Just s) -> do
      newId <- ThreadId <$> liftIO randomIO
      return $ maybe (pur id) (voidRunContinuationT . newThread (categoryId cat) newId t) (session z)
    _ -> return (pur id)


setNewThreadTitle :: (Zettel, InitialV) -> Category -> Text -> (Zettel, InitialV)
setNewThreadTitle model cat t =
  second (\i -> i { newThreadTitles = M.insert (categoryId cat) t (newThreadTitles i) })
  model


getNewThreadTitle :: (Zettel, InitialV) -> Category -> Text
getNewThreadTitle (_, i) cat = fromMaybe "" $ M.lookup (categoryId cat) (newThreadTitles i)


handleNewComment :: MonadJSM m => MonadUnliftIO m => ZettelEditor m => Continuation m (Zettel, ThreadV)
handleNewComment = Continuation . (id,) $ \(z, ThreadV t txt) -> do
  newId <- CommentId <$> liftIO randomIO
  return $ maybe (pur id) (voidRunContinuationT . newComment (threadId t) newId txt) (session z)


setCommentField :: (Zettel, ThreadV) -> Text -> (Zettel, ThreadV)
setCommentField (z, v) t = (z, v { commentField = t })


getToday :: MonadJSM m => m Day
getToday = do
  date  <- liftJSM $ eval ("new Date()" :: Text) >>= makeObject
  day   <- round <$> liftJSM (((date # ("getDay" :: Text) :: [JSVal] -> JSM JSVal) $ []) >>= valToNumber)
  month <- round <$> liftJSM (((date # ("getMonth" :: Text) :: [JSVal] -> JSM JSVal) $ []) >>= valToNumber)
  year  <- round <$> liftJSM (((date # ("getYear" :: Text) :: [JSVal] -> JSM JSVal) $ []) >>= valToNumber)
  return (fromGregorian year month day)


splitOn :: Eq a => a -> [a] -> Maybe ([a], [a])
splitOn x xs = do
  (ys, zs) <- splitOn' x [] xs
  return (Prelude.reverse ys, zs)
  where
    splitOn' :: Eq a => a -> [a] -> [a] -> Maybe ([a], [a])
    splitOn' _ _  [] = Nothing
    splitOn' x ys (z:zs)
      | x == z    = Just (ys, zs)
      | otherwise = splitOn' x (z:ys) zs


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


instance Semigroup Change where
  x <> y = ComposedChanges [x,y]


instance Monoid Change where
  mempty = ComposedChanges []


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
        ic <- o .: "categoryId"
        it <- o .: "threadId"
        t  <- o .: "title"
        return (NewThread ic it t)

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
        f <- o .: "fromThreadId"
        t <- o .: "toThreadId"
        c <- o .: "removeCommentId"
        return (RemoveComment f t c)

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
  toJSON (NewThread ic it t) = object [ "categoryId" .= ic, "threadId" .= it, "title" .= t]
  toJSON (NewComment t i x) = object [ "threadId" .= t, "commentId" .= i, "text" .= x]
  toJSON (NewEdit i x) = object [ "commentId" .= i, "text" .= x ]
  toJSON (AddThreadToCategory c t) = object [ "toCategoryId" .= c, "addThreadId" .= t ]
  toJSON (RemoveThreadFromCategory c t) = object [ "fromCategoryId" .= c, "removeThreadId" .= t ]
  toJSON (RetitleCategory f t r) = object [ "fromCategoryId" .= f, "toCategoryId" .= t, "retitle" .= r ]
  toJSON (RetitleThread f t r) = object [ "fromThreadId" .= f, "toThreadId" .= t, "retitle" .= r ]
  toJSON (RemoveComment f t c) = object [ "fromThreadId" .= f, "toThreadId" .= t, "removeCommentId" .= c ]
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


instance (Monad m, ZettelEditor m) => ZettelEditor (ParDiffT model m) where
  saveChange c s = lift $ saveChange c s
  getDatabase s = lift $ getDatabase s
  login u p = lift $ login u p


(saveChangeM :<|> getDatabaseM :<|> loginM) = client (Proxy @API)


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
  saveChange c s = runXHR App $ saveChangeM (c, sessionId s)
  getDatabase s = runXHR App $ getDatabaseM s
  login u p = runXHR App $ loginM u p


instance ( ZettelEditor m
         , MonadUnliftIO m
         , MonadJSM m
         ) => ZettelEditor (ContinuationT Zettel m) where
  saveChange c s = ContinuationT $ do
    void . forkIO $ saveChange c s
    d <- getToday
    return . ((),) $ pur (applyChange (sessionUser s) d c)
  getDatabase s = lift $ getDatabase s
  login u p = lift $ login u p


instance ( ZettelEditor m 
         , MonadUnliftIO m
         , MonadJSM m
         ) => ZettelEditor (ContinuationT InitialV m) where
  saveChange (NewCategory _ _) _ = ContinuationT . return $
    ((), pur (\v -> v { newCategoryTitle = "" }))
  saveChange (NewThread ic _ _) _ = ContinuationT . return $
    ((), pur (\v -> v { newThreadTitles = M.insert ic "" (newThreadTitles v) }))
  saveChange (TrashCategory i) _ = ContinuationT . return $
    ((), pur (\v -> v { newThreadTitles = M.delete i (newThreadTitles v) }))
  saveChange (UntrashCategory i) _ = ContinuationT . return $
    ((), pur (\v -> v { newThreadTitles = M.insert i "" (newThreadTitles v) }))
  saveChange _ _ = return ()
  getDatabase s = lift $ getDatabase s
  login u p = lift $ login u p


instance ( ZettelEditor m
         , MonadUnliftIO m
         , MonadJSM m
         )  => ZettelEditor (ContinuationT ThreadV m) where
  saveChange (NewComment _ _ _) _ = ContinuationT . return $
    ((), pur (\v -> v { commentField = "" }))
  saveChange _ _ = return ()
  getDatabase s = lift $ getDatabase s
  login u p = lift $ login u p


instance ( ZettelEditor m
         , ZettelEditor (ContinuationT a m) 
         , ZettelEditor (ContinuationT b m)
         , MonadJSM m
         , MonadUnliftIO m
         ) => ZettelEditor (ContinuationT (a, b) m) where
  saveChange c s = ContinuationT . return . ((),)
                 $ leftC  (voidRunContinuationT $ saveChange c s)
                <> rightC (voidRunContinuationT $ saveChange c s)
  getDatabase s = lift $ getDatabase s
  login u p = lift $ login u p


instance Routed SPA Route where
  redirect = \case
    InitialRoute -> Redirect (Proxy @("app" :> Raw)) id
    ThreadRoute tid -> Redirect (Proxy @("app" :> "thread" :> Capture "id" ThreadId :> Raw)) ($ tid)
