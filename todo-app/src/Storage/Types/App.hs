module Storage.Types.App where

import Data.Text
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Servant

data Env = 
  Env 
  {
    config :: Text
  } deriving (Show)

type FlowHandler = ReaderT Env (ExceptT ServerError IO)