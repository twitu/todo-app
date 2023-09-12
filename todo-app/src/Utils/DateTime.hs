module Utils.DateTime where

import qualified Data.Time as DT
import qualified Data.UUID as UUID
import qualified Flow as F

getTimeIST :: DT.UTCTime -> DT.LocalTime
getTimeIST = DT.utcToLocalTime ist

ist :: DT.TimeZone
ist = DT.TimeZone 330 False "IST"

getCurrentTimeIST :: IO DT.LocalTime
getCurrentTimeIST = DT.utcToLocalTime ist <$> DT.getCurrentTime
