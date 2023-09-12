module Storage.Types.App where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Servant
import qualified Config.Types as Conf

data Env = 
  Env 
  {
    config :: Conf.Config
  } deriving (Show)

type FlowHandler = ReaderT Env (ExceptT ServerError IO)