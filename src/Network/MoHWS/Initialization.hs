module Network.MoHWS.Initialization where

import qualified Network.MoHWS.Module.Description as ModuleDesc

data T body ext =
   Cons {
      moduleList :: [ModuleDesc.T body ext],
      configurationExtensionDefault :: ext
   }
