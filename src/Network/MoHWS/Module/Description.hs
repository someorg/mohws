{- |
Copyright: 2006, Bjorn Bringert
Copyright: 2009, Henning Thielemann
-}
module Network.MoHWS.Module.Description where

import qualified Network.MoHWS.Module as Module
import qualified Network.MoHWS.Configuration.Parser as ConfigParser
import qualified Network.MoHWS.Server.Context as ServerContext
import qualified Data.Accessor.Basic as Accessor


data T body ext = Cons
   {
      name :: String,
      load :: ServerContext.T ext -> IO (Module.T body),
      configParser :: ConfigParser.T () ext,
      setDefltConfig :: ext -> ext
   }

empty :: T body ext
empty = Cons
   {
      name = "<unnamed module>",
      load = const $ return Module.empty,
      configParser = fail "no parser available", -- identity element for 'Parsec.choice'
      setDefltConfig = id
   }

lift :: Accessor.T fullExt partExt -> T body partExt -> T body fullExt
lift acc desc = Cons
   {
      name = name desc,
      load = load desc . fmap (Accessor.get acc),
      configParser = ConfigParser.lift acc $ configParser desc,
      setDefltConfig = Accessor.modify acc (setDefltConfig desc)
   }
