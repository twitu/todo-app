module Middleware.Middleware where

import qualified Network.Wai as NW
import qualified Data.ByteString.Char8 as CH
import Data.IORef

customMiddleware ::  NW.Application -> NW.Application
customMiddleware app request response = do
  reqBody <-  readBodyFromStream id -- extracted the request body
  ichunks <- newIORef reqBody
  let rbody =
        atomicModifyIORef ichunks $ \case
          [] -> ([], CH.empty)
          x:y -> (y, x)
  let injectedRequest = request {NW.requestBody = putStrLn "reading" >> rbody} -- injected back the request
  app injectedRequest response
  where
    readBodyFromStream front = do
      bs <- NW.getRequestBodyChunk request
      if CH.null bs
        then return $ front []
        else readBodyFromStream $ front . (bs :)
