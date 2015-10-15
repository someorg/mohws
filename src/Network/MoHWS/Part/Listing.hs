{- |
Copyright: 2009, Henning Thielemann

Deliver a HTML document containing the contents of a directory.
-}
module Network.MoHWS.Part.Listing (Configuration, desc, ) where

import qualified Network.MoHWS.Module as Module
import qualified Network.MoHWS.Module.Description as ModuleDesc
import qualified Network.MoHWS.Server.Request as ServerRequest
import qualified Network.MoHWS.Server.Context as ServerContext
import qualified Network.MoHWS.HTTP.Header as Header
import qualified Network.MoHWS.HTTP.Request  as Request
import qualified Network.MoHWS.HTTP.Response as Response
import qualified Network.MoHWS.Stream as Stream

import qualified Network.MoHWS.Configuration as Config
import qualified Network.MoHWS.Configuration.Accessor as ConfigA
import qualified Network.MoHWS.Configuration.Parser as ConfigParser
import qualified Data.Accessor.Basic as Accessor
import Data.Accessor.Basic ((.>))

import qualified Text.Html as Html
import           Text.Html((<<), (+++))
import qualified Network.URI as URI
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (guard, )
import Data.List (sort, )
import Control.Monad.Trans.Maybe (MaybeT, )
import Network.MoHWS.Utility (hasTrailingSlash, statFile, )

import qualified System.Directory as Dir
import System.Posix (isDirectory, )



desc :: (Stream.C body) => ModuleDesc.T body Configuration
desc =
   ModuleDesc.empty {
      ModuleDesc.name = "directorylisting",
      ModuleDesc.load = return . funs,
      ModuleDesc.configParser = parser,
      ModuleDesc.setDefltConfig = const defltConfig
   }

data Configuration =
   Configuration {
      listing_ :: Bool
   }

defltConfig :: Configuration
defltConfig =
   Configuration {
      listing_ = True
   }

listing :: Accessor.T Configuration Bool
listing =
   Accessor.fromSetGet (\x c -> c{listing_ = x}) listing_

parser :: ConfigParser.T st Configuration
parser =
   ConfigParser.field "directorylisting" p_listing

p_listing :: ConfigParser.T st Configuration
p_listing =
   ConfigParser.set (ConfigA.extension .> listing) $ ConfigParser.bool

funs :: (Stream.C body) => ServerContext.T Configuration -> Module.T body
funs st =
   Module.empty {
      Module.handleRequest = handleRequest st
   }

handleRequest :: (Stream.C body) =>
   ServerContext.T Configuration -> ServerRequest.T body -> MaybeT IO (Response.T body)
handleRequest st req =
   let conf = ServerContext.config st
       dir  = ServerRequest.serverFilename req
       uri  = Request.uri $ ServerRequest.clientRequest req
   in  do -- liftIO $ print dir
          guard $ listing_ $ Config.extension conf
          guard =<< (fmap isDirectory $ statFile $ dir)
          guard $ hasTrailingSlash $ URI.uriPath uri
          files <- liftIO $ Dir.getDirectoryContents dir
          return $ htmlResponse conf uri $ htmlList $
             sort $ filter (not . flip elem [".", ".."]) $ files

htmlList :: [String] -> Html.Html
htmlList =
   Html.unordList .
   map (\s -> (Html.anchor << s) Html.! [Html.href s])

htmlResponse :: (Stream.C body) =>
   Config.T ext -> URI.URI -> Html.Html -> Response.T body
htmlResponse conf addr body =
   Response.makeOk
      conf
      True
      (Header.group [Header.makeContentType "text/html"])
      (Response.bodyWithSizeFromString $
       Stream.fromString (Config.chunkSize conf) $
       Html.renderHtml $
       htmlDoc ("Directory listing of " ++ show addr) body)

htmlDoc :: String -> Html.Html -> Html.Html
htmlDoc title body =
   Html.header
      (Html.meta Html.! [Html.httpequiv "content-type",
                         Html.content "text/html; charset=ISO-8859-1"]
       +++
       Html.thetitle << title)
   +++
   Html.body body
