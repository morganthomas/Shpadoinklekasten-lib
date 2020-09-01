{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}


module View where


import           Control.Monad.IO.Class
import           Control.PseudoInverseCategory (piiso)
import           Control.ShpadoinkleContinuation (runContinuation)
import           Data.Text
import           Shpadoinkle
import           Shpadoinkle.Html
import           Shpadoinkle.Router
import           UnliftIO
import           Prelude hiding (div)

import           Types

import           View.Login
import           View.Initial
import           View.Thread


default (ClassList)

template :: Monad m => Text -> HtmlM m a -> HtmlM m a
template js v = html_
  [ head_
    [ link' [ rel "stylesheet", href "https://cdn.usebootstrap.com/bootstrap/4.3.1/css/bootstrap.min.css" ]
    , meta [ charset "ISO-8859-1" ] []
    , script [ type' "text/javascript" ] [ text js ] ]
  , body_ [ v ] ]


viewRouter :: ZettelController m => Route -> IO (HtmlM m ViewModel)
viewRouter r = let model = initialModel r emptyZettel
  in view . ($ model) <$> runContinuation (router r) model


view :: ZettelController m => ViewModel -> HtmlM m ViewModel
view model = viewContainer model . pimap coproductIsoViewModel . viewCases $ viewModelToCoproduct model


viewContainer :: Monad m => ViewModel -> HtmlM m a -> HtmlM m a
viewContainer model v =
  div [class' "container-fluid s11k-app"] [
      h1_ [ "Shpadoinklekasten" ],
      div [class' "s11k-login-status"] [ text $ maybe "Not logged in" (("Logged in as " <>) . unUserId) (whoAmI (fst model)) ],
      div [class' "s11k-view"] [ v ] ]


viewCases :: ZettelController m => ViewModelCoproduct -> HtmlM m ViewModelCoproduct
viewCases = initialView `eitherH` threadView `eitherH` loginView

