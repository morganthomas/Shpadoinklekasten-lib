{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}


module View.Components where

import           Data.Text
import           Data.Time.Calendar


dateView :: Day -> Text
dateView = pack . (\(y, m, d) -> show y <> "-" <> show m <> "-" <> show d) . toGregorian
