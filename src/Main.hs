module Main where

import qualified Network.MoHWS.Server as Server
import qualified Network.MoHWS.Initialization as Init
import qualified Network.MoHWS.Initialization.Standard as Std
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main =
   Server.main (Std.init :: Init.T B.ByteString Std.Extension)
