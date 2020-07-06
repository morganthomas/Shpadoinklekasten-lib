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


module Types.ViewModel where


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


type ViewModel = (Zettel, View)


type ViewModelCoproduct = Either (Either (Zettel, InitialV) (Zettel, ThreadV)) (Zettel, LoginV)


coproductToViewModel :: ViewModelCoproduct -> ViewModel
coproductToViewModel (Left (Left (z, i))) = (z, InitialView i)
coproductToViewModel (Left (Right (z, t))) = (z, ThreadView t)
coproductToViewModel (Right (z, l)) = (z, LoginView l)


viewModelToCoproduct :: ViewModel -> ViewModelCoproduct
viewModelToCoproduct (z, InitialView i) = Left (Left (z, i))
viewModelToCoproduct (z, ThreadView t) = Left (Right (z, t))
viewModelToCoproduct (z, LoginView l) = Right (z, l)


coproductIsoViewModel :: EndoIso ViewModelCoproduct ViewModel
coproductIsoViewModel = piiso coproductToViewModel viewModelToCoproduct


initialViewModel :: Zettel -> InitialV
initialViewModel z = InitialV "" (M.fromList $ (,"") <$> M.keys (categories z))


initialModel :: Route -> Zettel -> ViewModel
initialModel InitialRoute z = (z, InitialView (initialViewModel z))
initialModel (ThreadRoute tid) z = case M.lookup tid (threads z) of
  Just t  -> (z, ThreadView (ThreadV t ""))
  Nothing -> (z, InitialView (initialViewModel z))
initialModel LoginRoute z = (z, LoginView (LoginV "" ""))


setUserId :: (Zettel, LoginV) -> Text -> (Zettel, LoginV)
setUserId (z, LoginV _ p) u = (z, LoginV u p)


setPassword :: (Zettel, LoginV) -> Text -> (Zettel, LoginV)
setPassword (z, LoginV u _) p = (z, LoginV u p)


setNewCategoryTitle :: (Zettel, InitialV) -> Text -> (Zettel, InitialV)
setNewCategoryTitle (z, i) t = (z, i { newCategoryTitle = t })


setNewThreadTitle :: (Zettel, InitialV) -> Category -> Text -> (Zettel, InitialV)
setNewThreadTitle model cat t =
  second (\i -> i { newThreadTitles = M.insert (categoryId cat) t (newThreadTitles i) })
  model


getNewThreadTitle :: (Zettel, InitialV) -> Category -> Text
getNewThreadTitle (_, i) cat = fromMaybe "" $ M.lookup (categoryId cat) (newThreadTitles i)


setCommentField :: (Zettel, ThreadV) -> Text -> (Zettel, ThreadV)
setCommentField (z, v) t = (z, v { commentField = t })


