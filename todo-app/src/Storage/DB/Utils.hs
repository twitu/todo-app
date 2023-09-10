module Storage.DB.Utils where

import Data.Aeson
import Data.List  as DL

stripAllLensPrefixOptions :: Options
stripAllLensPrefixOptions = defaultOptions {fieldLabelModifier = dropPrefix}
  where
    dropPrefix :: String -> String
    dropPrefix field =
      if not $ null field
        then dropWhile (== DL.head field) field
        else field