module Exception where

import Control.Exception

data ThrowException = Exception String
  deriving (Show)

instance Exception ThrowException
