module Flow where

import Control.Monad.Trans.Reader
import Data.Text
import Prelude


type Flow = ReaderT Env IO

data Env = 
  Env 
  {
    config :: Text
  } deriving (Show)