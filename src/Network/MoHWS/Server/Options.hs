module Network.MoHWS.Server.Options (
   T(Cons),
   serverRoot, configPath, inServerRoot,
   parse,
   ) where

import System.Console.GetOpt
          (getOpt, usageInfo,
           OptDescr(Option), ArgDescr(ReqArg), ArgOrder(Permute), )
import qualified System.FilePath as FilePath


data T =
   Cons {
      configFile :: FilePath,
      serverRoot :: FilePath
   }


options :: [OptDescr (T -> T)]
options =
  Option ['f'] ["config"] (ReqArg (\path opt -> opt{configFile=path}) "filename")
     ("default: \n" ++ show ("<server-root>/" ++ defltConfigFile)) :
  Option ['d'] ["server-root"] (ReqArg (\path opt -> opt{serverRoot=path}) "directory")
     ("default: " ++ show defltServerRoot) :
  []

usage :: String
usage = "usage: hws [option...]"

defltConfigFile :: FilePath
defltConfigFile = "conf/httpd.conf"

defltServerRoot :: FilePath
defltServerRoot = "."

deflt :: T
deflt =
   Cons {
      configFile = defltConfigFile,
      serverRoot = defltServerRoot
   }

configPath :: T -> FilePath
configPath opts =
   inServerRoot opts (configFile opts)

inServerRoot :: T -> FilePath -> FilePath
inServerRoot opts =
   FilePath.combine (serverRoot opts)

-- returns error message or options
parse :: [String] -> Either String T
parse args =
    case getOpt Permute options args of
      (flags, [], [])   -> Right $ foldl (flip ($)) deflt flags
      (_,     _,  errs) -> Left (concat errs ++ "\n"
                                 ++ usageInfo usage options)
