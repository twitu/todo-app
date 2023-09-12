module Flow where

import Control.Monad.Trans.Reader
import Prelude
import Storage.Types.App
import qualified Config.Types as Conf

type Flow = ReaderT Env IO

getConfig :: Flow Conf.Config
getConfig = do
  Env {..} <- ask
  return config