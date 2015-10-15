module Network.MoHWS.Logger.Level (T(..)) where

import qualified Data.Map as Map


data T =
     Debug
   | Info
   | Notice
   | Warn
   | Error
   | Crit
   | Alert
   | Emerg
     deriving (Eq,Ord,Enum,Bounded)


names :: Map.Map T String
names =
   Map.fromList $
   (Debug,  "debug")  :
   (Info,   "info")   :
   (Notice, "notice") :
   (Warn,   "warn")   :
   (Error,  "error")  :
   (Crit,   "crit")   :
   (Alert,  "alert")  :
   (Emerg,  "emerg")  :
   []

instance Show T where
    show l =
       Map.findWithDefault
          (error $ "LogLevel.names is incomplete") l names

instance Read T where
    readsPrec _ s = [ (l,"") | (l,n) <- Map.toList names, n == s ]
