{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}


module ClientMain ( main ) where


import           Data.Text
import           Language.Javascript.JSaddle (liftJSM, eval)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html
import           Shpadoinkle.Html.LocalStorage
import           Shpadoinkle.Router

import           Types
import           View

default          ( Text )


initialState :: MonadJSM m => ZettelEditor m => Route -> m ViewModel
initialState r = do
  msid <- getStorage (LocalStorageKey "session")
  case msid of
    Just sid -> initialModel r <$> getDatabase sid
    Nothing -> do
      liftJSM $ eval "window.localStorage.removeItem('session')"
      return $ initialModel LoginRoute emptyZettel


main :: IO ()
main = runJSorWarp 8082 $ do
  fullPageSPA @SPA runApp runParDiff initialState view getBody (return . router) routes
