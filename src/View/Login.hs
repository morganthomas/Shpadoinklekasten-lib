{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}


module View.Login where

import           Prelude hiding (div)

import           Shpadoinkle
import           Shpadoinkle.Html

import           Types

default (ClassList)


loginView :: ZettelController m => (Zettel, LoginV) -> HtmlM m (Zettel, LoginV)
loginView model@(z,l) =
  div [class' "s11k-login form-group"] [
    input' [ class' "form-control", type' "text", placeholder "User ID", onInput (setUserId model) ],
    input' [ class' "form-control", type' "password", placeholder "Password", onInput (setPassword model) ],
    button [ class' "btn btn-primary", onClickE handleLogin ] [ text "Login" ] ]

