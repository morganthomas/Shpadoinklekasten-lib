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


module Types.Controller where


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

import           Types.Model
import           Types.ViewModel


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
            | MoveCategory CategoryId Int -- index to move it to
            | MoveComment ThreadId CommentId Int -- index to move it to
            | MoveThread CategoryId ThreadId Int -- index to move it to
            | NewRelationLabel RelationLabel
            | DeleteRelationLabel RelationLabel
            | NewRelation Relation
            | DeleteRelation Relation
            | ComposedChanges [Change]
            deriving (Eq, Show)


class ZettelEditor m where
  saveChange :: Change -> SessionId -> m ()
  getDatabase :: SessionId -> m Zettel
  login :: UserId -> PasswordHash -> m (Maybe Session)


type API =      "api" :> ReqBody' '[Required] '[JSON] Change
                      :> Header' '[Required] "session" SessionId
                      :> Post '[JSON] ()

           :<|> "api" :> Header' '[Required] "session" SessionId
                      :> Get '[JSON] Zettel

           :<|> "api" :> "login" :> Capture "id" UserId
                      :> ReqBody' '[Required] '[OctetStream] PasswordHash
                      :> Post '[JSON] (Maybe Session)


class MonadJSM m => ZettelController m where
  handleLogin :: Continuation m (Zettel, LoginV)
  reload :: Continuation m (Zettel, InitialV)
  handleNewCategory :: Continuation m (Zettel, InitialV)
  handleNewThread :: Category -> Continuation m (Zettel, InitialV)
  handleNewComment :: Continuation m (Zettel, ThreadV)
  handleOpenEdit :: CommentId -> Continuation m (Zettel, ThreadV)
  handleCancelEdit :: Continuation m (Zettel, ThreadV)
  handleSaveEdit :: Continuation m (Zettel, ThreadV)
  handleAddThreadToCategory :: CategoryId -> Continuation m (Zettel, ThreadV)
  handleRemoveThreadFromCategory :: CategoryId -> Continuation m (Zettel, ThreadV)
  handleRemoveComment :: CommentId -> Continuation m (Zettel, ThreadV)
  handleOpenRetitleCategory :: CategoryId -> Continuation m (Zettel, InitialV)
  handleCancelRetitleCategory :: Continuation m (Zettel, InitialV)
  handleSaveRetitleCategory :: Continuation m (Zettel, InitialV)
  handleOpenRetitleThread :: Continuation m (Zettel, ThreadV)
  handleCancelRetitleThread :: Continuation m (Zettel, ThreadV)
  handleSaveRetitleThread :: Continuation m (Zettel, ThreadV)
  handleTrashCategory :: CategoryId -> Continuation m (Zettel, InitialV)
  -- TODO untrash categories will need route / viewmodel additions for viewing category trash
  -- TODO also need route / viewmodel additions for viewing trashed threads
  handleSplitThread :: CommentId -> Continuation m (Zettel, ThreadV)
  -- TODO add comment or range to thread will need viewmodel addition
  handleMoveCategoryUp :: CategoryId -> Continuation m (Zettel, InitialV)
  handleMoveCategoryDown :: CategoryId -> Continuation m (Zettel, InitialV)
  handleMoveCommentUp :: CommentId -> Continuation m (Zettel, ThreadV)
  handleMoveCommentDown :: CommentId -> Continuation m (Zettel, ThreadV)
  handleMoveThreadUp :: CategoryId -> ThreadId -> Continuation m (Zettel, InitialV)
  handleMoveThreadDown :: CategoryId -> ThreadId -> Continuation m (Zettel, InitialV)
  handleAddRelationLabel :: Continuation m (Zettel, InitialV)
  handleDeleteRelationLabel :: RelationLabel -> Continuation m (Zettel, InitialV)
  -- TODO: add and delete relation will need viewmodel additions


type SPA = "app" :> Raw
      :<|> "app" :> "thread" :> Capture "id" ThreadId :> Raw
      :<|> "app" :> "login" :> Raw


routes :: SPA :>> Route
routes = InitialRoute :<|> ThreadRoute :<|> LoginRoute


router :: Monad m => Route -> Continuation m ViewModel
router InitialRoute = pur $
  \(z, _) -> case session z of
    Just _  -> (z, InitialView (InitialV "" emptyLabel Nothing (M.fromList ((,"") <$> M.keys (categories z)))))
    Nothing -> (z, LoginView (LoginV "" ""))
router (ThreadRoute tid) = pur $
  (\(z, v) -> case (session z, M.lookup tid (threads z)) of
                (Just _, Just _) -> (z, ThreadView (ThreadV tid "" Nothing Nothing))
                (Nothing, _) -> (z, LoginView (LoginV "" ""))
                _ -> (z, v))
router LoginRoute = pur $ \(z, _) -> (z, LoginView (LoginV "" ""))


invertChange :: Zettel -> Change -> Change

invertChange _ (NewCategory c _) = TrashCategory c

invertChange z (NewComment t c _) = RemoveComment t (nextThreadId z t) c

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

invertChange _ (TrashCategory c) = UntrashCategory c -- TODO: return to original position in category ordering

invertChange _ (UntrashCategory c) = TrashCategory c

invertChange z (SplitThread t f s c) = ComposedChanges $
  do thread <- maybeToList (M.lookup t (threads z))
     c <- categorization thread
     [RemoveThreadFromCategory c f, RemoveThreadFromCategory c s, AddThreadToCategory c t]

invertChange z (AddCommentToThread t c) = RemoveComment t (nextThreadId z t) c

invertChange z (AddCommentRangeToThread f s e t) =
  let t' = nextThreadId z t in ComposedChanges $
    do thread <- maybeToList (M.lookup f (threads z))
       c <- takeWhile (not . (== e)) . dropWhile (not . (== s)) $ threadCommentIds thread
       [RemoveComment t t' c]

invertChange z (MoveCategory c j) = ComposedChanges $
  do i <- maybeToList . findIndex (== c) $ categoryOrdering z
     return $ MoveCategory c i

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
  return $ z { categories = M.insert i (Category t i [] Nothing False) (categories z)
             , categoryOrdering = i : categoryOrdering z }

applyChange u d (NewThread ic it t) z = fromMaybe z $ do
  c <- M.lookup ic (categories z)
  guard . isNothing $ M.lookup it (threads z)
  return $ z { threads = M.insert it (Thread it t u d [] [ic] Nothing) (threads z)
             , categories = M.insert ic c { categoryThreadIds = categoryThreadIds c ++ [it] } (categories z) }

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
                            (categories z)
              , categoryOrdering = (\j -> if j == i then i' else j) <$> categoryOrdering z }

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
  return $ z { categories = M.insert ic c { categoryIsTrash = True } (categories z)
             , categoryOrdering = filter (/= ic) (categoryOrdering z) }

applyChange _ _ (UntrashCategory ic) z = fromMaybe z $ do
  c <- M.lookup ic (categories z)
  return $ z { categories = M.insert ic c { categoryIsTrash = False } (categories z)
              , categoryOrdering = ic : categoryOrdering z }

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

applyChange _ _ (MoveCategory c i) z = fromMaybe z $ do
  _ <- M.lookup c (categories z)
  return $ z { categoryOrdering = categoryOrdering z & filter (/= c) & insertAt i c }

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


newCategory :: ZettelEditor m => CategoryId -> Text -> SessionId -> m ()
newCategory i t = saveChange (NewCategory i t)


newThread :: ZettelEditor m => CategoryId -> ThreadId -> Text -> SessionId -> m ()
newThread ic it t = saveChange (NewThread ic it t)


newComment :: ZettelEditor m => ThreadId -> CommentId -> Text -> SessionId -> m ()
newComment i c t = saveChange (NewComment i c t)


newEdit :: ZettelEditor m => CommentId -> Text -> SessionId -> m ()
newEdit i t = saveChange (NewEdit i t)


addThreadToCategory :: ZettelEditor m => CategoryId -> ThreadId -> SessionId -> m ()
addThreadToCategory c t = saveChange (AddThreadToCategory c t)


removeThreadFromCategory :: ZettelEditor m => CategoryId -> ThreadId -> SessionId -> m ()
removeThreadFromCategory c t = saveChange (RemoveThreadFromCategory c t)


retitleCategory :: ZettelEditor m => CategoryId -> CategoryId -> Text -> SessionId -> m ()
retitleCategory f t x = saveChange (RetitleCategory f t x)


retitleThread :: ZettelEditor m => ThreadId -> ThreadId -> Text -> SessionId -> m ()
retitleThread f t x = saveChange (RetitleThread f t x)


removeComment :: ZettelEditor m => ThreadId -> ThreadId -> CommentId -> SessionId -> m ()
removeComment f t c = saveChange (RemoveComment f t c)


trashCategory :: ZettelEditor m => CategoryId -> SessionId -> m ()
trashCategory c = saveChange (TrashCategory c)


untrashCategory :: ZettelEditor m => CategoryId -> SessionId -> m ()
untrashCategory c = saveChange (UntrashCategory c)


splitThread :: ZettelEditor m => ThreadId -> ThreadId -> ThreadId -> CommentId -> SessionId -> m ()
splitThread f a b c = saveChange (SplitThread f a b c)


addCommentToThread :: ZettelEditor m => ThreadId -> CommentId -> SessionId -> m ()
addCommentToThread t c = saveChange (AddCommentToThread t c)


addCommentRangeToThread :: ZettelEditor m => ThreadId -> CommentId -> CommentId -> ThreadId -> SessionId -> m ()
addCommentRangeToThread f s e t = saveChange (AddCommentRangeToThread f s e t)


moveCategory :: ZettelEditor m => CategoryId -> Int -> SessionId -> m ()
moveCategory c i = saveChange (MoveCategory c i)


moveComment :: ZettelEditor m => ThreadId -> CommentId -> Int -> SessionId -> m ()
moveComment t c i = saveChange (MoveComment t c i)


moveThread :: ZettelEditor m => CategoryId -> ThreadId -> Int -> SessionId -> m ()
moveThread c t i = saveChange (MoveThread c t i)


newRelationLabel :: ZettelEditor m => RelationLabel -> SessionId -> m ()
newRelationLabel l = saveChange (NewRelationLabel l)


deleteRelationLabel :: ZettelEditor m => RelationLabel -> SessionId -> m ()
deleteRelationLabel l = saveChange (DeleteRelationLabel l)


newRelation :: ZettelEditor m => Relation -> SessionId -> m ()
newRelation r = saveChange (NewRelation r)


deleteRelation :: ZettelEditor m => Relation -> SessionId -> m ()
deleteRelation r = saveChange (DeleteRelation r)


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


instance ( Monad m
         , MonadJSM m
         , MonadUnliftIO m
         , ZettelEditor m
         ) => ZettelController m where
  handleLogin = kleisliT $ \(z, LoginV u p) -> do
    h   <- lift (liftJSM (hash p))
    commit . pur . second . const $ LoginV "" ""
    res <- lift $ login (UserId u) h
    case res of
      Just s -> do
        commit . pur . first $ \z' -> z' { session = Just s }
        commit . kleisliT $ \(z',v) -> do
          lift $ setStorage "session" (sessionId s)
          lift $ navigate @SPA InitialRoute
          z'' <- lift . getDatabase $ sessionId s
          commit . pur . first $ const z''
          lift $ navigate @SPA InitialRoute
      Nothing -> return () -- TODO: show message saying login failed
  
  reload = kleisli $ \(z,_) ->
    case session z of
      Just s -> do
        z' <- getDatabase (sessionId s)
        return . pur $ const (z', initialViewModel z')
      Nothing -> return done
  
  handleNewCategory = kleisli $ \(z,i) -> do
    newId <- CategoryId <$> liftIO randomIO
    return $ maybe done (voidRunContinuationT . newCategory newId (newCategoryTitle i)) (sessionId <$> session z)
  
  handleNewThread cat = kleisliT $ \(z,i) ->
    case (M.lookup (categoryId cat) (newThreadTitles i), session z) of
      (Just t, Just s) -> do
        newId <- ThreadId <$> lift (liftIO randomIO)
        commit . pur . second $ \v -> v { newThreadTitles = newThreadTitles v & M.insert (categoryId cat) "" }
        newThread (categoryId cat) newId t (sessionId s)
      _ -> return ()
  
  handleNewComment = kleisliT $ \(z, ThreadV tid txt _ _) -> do
    newId <- CommentId <$> lift (liftIO randomIO)
    commit . pur . second $ \v -> v { commentField = "" }
    maybe (return ()) (newComment tid newId txt) (sessionId <$> session z)

  handleOpenEdit c = pur (second (\v -> v { editCommentField = Just (c, "") } ) )

  handleCancelEdit = pur (second (\v -> v { editCommentField = Nothing } ) )

  handleSaveEdit = kleisliT $ \(z, ThreadV tid _ f _) ->
    case (f, session z, M.lookup tid (threads z)) of
      (Just (c, txt), Just s, Just _) -> do
        newEdit c txt (sessionId s)
        commit . pur . second $ \v -> v { editCommentField = Nothing }
      _ -> return ()

  handleAddThreadToCategory cid = kleisliT $ \(z, ThreadV tid _ _ _) ->
    maybe (return ()) (addThreadToCategory cid tid) (sessionId <$> session z)

  handleRemoveThreadFromCategory cid = kleisliT $ \(z, ThreadV tid _ _ _) ->
    maybe (return ()) (removeThreadFromCategory cid tid) (sessionId <$> session z)

  handleRemoveComment cid = kleisliT $ \(z, ThreadV tid _ _ _) ->
    case session z of
      Just s -> do
        newId <- ThreadId <$> lift (liftIO randomIO)
        removeComment tid newId cid (sessionId s)
        commit . pur . second $ \v -> v { viewedThread = newId }

  handleOpenRetitleCategory cid = pur . second $ \v -> v { retitleCategoryField = Just (cid, "") }

  handleCancelRetitleCategory = pur . second $ \v -> v { retitleCategoryField = Nothing }

  handleSaveRetitleCategory = kleisliT $ \(z, v) -> do
    newId <- CategoryId <$> lift (liftIO randomIO)
    maybe (return ()) (uncurry (uncurry (uncurry retitleCategory))) $ do
      (cid, txt) <- retitleCategoryField v
      s          <- sessionId <$> session z
      return (((cid, newId), txt), s)

  handleOpenRetitleThread = pur . second $ \v -> v { retitleThreadField = Just "" }

  handleCancelRetitleThread = pur . second $ \v -> v { retitleThreadField = Nothing }

  handleSaveRetitleThread = kleisliT $ \(z, v) -> do
    newId <- ThreadId <$> lift (liftIO randomIO)
    maybe (return ()) (uncurry (uncurry (uncurry retitleThread))) $ do
      txt <- retitleThreadField v
      s   <- sessionId <$> session z
      return (((viewedThread v, newId), txt), s)

  handleTrashCategory cid = kleisliT $ \(z, _) ->
    maybe (return ()) (trashCategory cid) (sessionId <$> session z)

  handleSplitThread cid = kleisliT $ \(z, ThreadV tid _ _ _) ->
    case session z of
      Just s -> do
        newIdA <- ThreadId <$> lift (liftIO randomIO)
        newIdB <- ThreadId <$> lift (liftIO randomIO)
        splitThread tid newIdA newIdB cid (sessionId s)
      Nothing -> return ()

  handleMoveCategoryUp cid = kleisliT $ \(z, _) ->
    maybe (return ()) (uncurry (uncurry moveCategory)) $ do
      i <- findIndex (== cid) (categoryOrdering z)
      s <- sessionId <$> session z
      return ((cid, i-1), s)

  handleMoveCategoryDown cid = kleisliT $ \(z, _) ->
    maybe (return ()) (uncurry (uncurry moveCategory)) $ do
      i <- findIndex (== cid) (categoryOrdering z)
      s <- sessionId <$> session z
      return ((cid, i+1), s)

  handleMoveCommentUp cid = kleisliT $ \(z, ThreadV tid _ _ _) ->
    maybe (return ()) (uncurry (uncurry (uncurry moveComment))) $ do
      t <- M.lookup tid (threads z)
      i <- findIndex (== cid) (threadCommentIds t)
      s <- sessionId <$> session z
      return (((threadId t, cid), i-1), s)

  handleMoveCommentDown cid = kleisliT $ \(z, ThreadV tid _ _ _) ->
    maybe (return ()) (uncurry (uncurry (uncurry moveComment))) $ do
      t <- M.lookup tid (threads z)
      i <- findIndex (== cid) (threadCommentIds t)
      s <- sessionId <$> session z
      return (((threadId t, cid), i+1), s)

  handleMoveThreadUp cid tid = kleisliT $ \(z, _) ->
    maybe (return ()) (uncurry (uncurry (uncurry moveThread))) $ do
      cat <- M.lookup cid (categories z)
      i   <- findIndex (== tid) (categoryThreadIds cat)
      s   <- sessionId <$> session z
      return (((cid, tid), i), s)

  handleMoveThreadDown cid tid = kleisliT $ \(z, _) ->
    maybe (return ()) (uncurry (uncurry (uncurry moveThread))) $ do
      cat <- M.lookup cid (categories z)
      i   <- findIndex (== tid) (categoryThreadIds cat)
      s   <- sessionId <$> session z
      return (((cid, tid), i), s)

  handleAddRelationLabel = kleisliT $ \(z,v) -> do
    commit . pur . second $ \v' -> v' { newRelationLabelField = emptyLabel }
    maybe (return ()) (newRelationLabel (newRelationLabelField v)) (sessionId <$> session z)

  handleDeleteRelationLabel l = kleisliT $ \(z, _) ->
    maybe (return ()) (deleteRelationLabel l) (sessionId <$> session z)


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
    <|> parseMoveCategory o <|> parseMoveComment o <|> parseMoveThread o <|> parseNewRelationLabel o
    <|> parseDeleteRelationLabel o <|> parseNewRelation o <|> parseDeleteRelation o) x
    <|> (ComposedChanges <$> parseJSON x)

    where
      parseNewCategory o = do
        i <- o .: "categoryId"
        t <- o .: "categoryTitle"
        return (NewCategory i t)

      parseNewThread o = do
        ic <- o .: "categoryId"
        it <- o .: "threadId"
        t  <- o .: "threadTitle"
        return (NewThread ic it t)

      parseNewComment o = do
        t <- o .: "threadId"
        i <- o .: "commentId"
        x <- o .: "text"
        return (NewComment t i x)

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


      parseMoveCategory o = do
        c <- o .: "moveCategoryId"
        i <- o .: "toIndex"
        return (MoveCategory c i)

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
  toJSON (NewCategory i t) = object [ "categoryId" .= i, "categoryTitle" .= t ]
  toJSON (NewThread ic it t) = object [ "categoryId" .= ic, "threadId" .= it, "threadTitle" .= t]
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
  toJSON (MoveCategory c i) = object [ "moveCategoryId" .= c, "toIndex" .= i ]
  toJSON (MoveComment t c i) = object [ "inThreadId" .= t
                                      , "moveCommentId" .= c
                                      , "toIndex" .= i ]
  toJSON (NewRelationLabel l) = object [ "relationLabel" .= l ]
  toJSON (DeleteRelationLabel l) = object [ "deleteRelationLabel" .= l ]
  toJSON (NewRelation r) = object [ "relation" .= r ]
  toJSON (DeleteRelation r) = object [ "deleteRelation" .= r ]


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
  saveChange c s = runXHR App $ saveChangeM c s
  getDatabase s = runXHR App $ getDatabaseM s
  login u p = runXHR App $ loginM u p


instance ( ZettelEditor m
         , MonadUnliftIO m
         , MonadJSM m
         ) => ZettelEditor (ContinuationT Zettel m) where
  saveChange c s = ContinuationT $ do
    void . forkIO $ saveChange c s
    d <- getToday
    return . ((),) $ pur
      (\z -> case session z of
        Just s -> applyChange (sessionUser s) d c z
        Nothing -> z)
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
         ) => ZettelEditor (ContinuationT ThreadV m) where
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
