{- |
Copyright: 2006, Bjorn Bringert
Copyright: 2009, Henning Thielemann
-}
module Network.MoHWS.Initialization.Standard (Extension, init, ) where

import qualified Network.MoHWS.Module.Description as ModuleDesc
import qualified Network.MoHWS.Initialization as Init
import qualified Network.MoHWS.Stream as Stream
import qualified Data.Accessor.Basic as Accessor

import qualified Network.MoHWS.Part.UserDirectory as UserDir
import qualified Network.MoHWS.Part.VirtualHost   as VirtualHost
import qualified Network.MoHWS.Part.AddSlash      as AddSlash
import qualified Network.MoHWS.Part.Index         as Index
import qualified Network.MoHWS.Part.CGI           as CGI
import qualified Network.MoHWS.Part.File          as File
import qualified Network.MoHWS.Part.Listing       as Listing

import Prelude hiding (init, )


data Extension =
   Extension {
      userDir     :: UserDir.Configuration,
      virtualHost :: VirtualHost.Configuration,
      addSlash    :: AddSlash.Configuration,
      index       :: Index.Configuration,
      cgi         :: CGI.Configuration,
      file        :: File.Configuration,
      listing     :: Listing.Configuration
   }

lift ::
   (partExt -> fullExt -> fullExt) -> (fullExt -> partExt) ->
   ModuleDesc.T body partExt -> ModuleDesc.T body fullExt
lift set get =
   ModuleDesc.lift (Accessor.fromSetGet set get)

moduleList :: (Stream.C body) => [ModuleDesc.T body Extension]
moduleList =
   lift (\x ext -> ext{userDir     = x}) userDir     UserDir.desc :
   lift (\x ext -> ext{virtualHost = x}) virtualHost VirtualHost.desc :
   lift (\x ext -> ext{addSlash    = x}) addSlash    AddSlash.desc :
   lift (\x ext -> ext{index       = x}) index       Index.desc :
   lift (\x ext -> ext{cgi         = x}) cgi         CGI.desc :
   lift (\x ext -> ext{file        = x}) file        File.desc :
   lift (\x ext -> ext{listing     = x}) listing     Listing.desc :
   []

init :: (Stream.C body) => Init.T body Extension
init =
   Init.Cons {
      Init.moduleList = moduleList,
      Init.configurationExtensionDefault =
         Extension
            (error "uninitialized userDir extension")
            (error "uninitialized virtualHost extension")
            (error "uninitialized addSlash extension")
            (error "uninitialized index extension")
            (error "uninitialized cgi extension")
            (error "uninitialized file extension")
            (error "uninitialized listing extension")
   }
