module Main (main) where

import Prelude hiding (FilePath)
import Control.Applicative ((<$>), (<*>), pure)
import System.Environment (getArgs)
import Control.Error (readMay)
import Network.URI (parseAbsoluteURI, URI(..), URIAuth(..))
import System.IO (hPutStrLn, stderr)
import Filesystem.Path.CurrentOS (FilePath)
import Filesystem (getWorkingDirectory)
import Database.SQLite.Simple (open)
import OpenSSL (withOpenSSL)
import qualified Vogogo as Vgg

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)

import Network.Wai.Dispatch
import Routes

addTrailingSlash :: URI -> URI
addTrailingSlash u@(URI {uriPath = []}) = u {uriPath = "/"}
addTrailingSlash u@(URI {uriPath = p})
	| last p == '/' = u
	| otherwise = u {uriPath = p ++ "/"}

withTrailingSlash :: String -> Maybe URI
withTrailingSlash s = addTrailingSlash <$> parseAbsoluteURI s

staticRoot :: FilePath -> Application
staticRoot = staticApp . defaultWebAppSettings

mountAt :: [String] -> Maybe (URI, Int)
mountAt [] = (,) <$> withTrailingSlash "http://localhost:3000/" <*> pure 3000
mountAt (root:port:_) = (,) <$> withTrailingSlash root <*> readMay port
mountAt (root:_) = do
	uri@(URI {uriAuthority = Just (URIAuth {uriPort = ':':sport})}) <-
		withTrailingSlash root
	port <- readMay sport
	return (uri, port)

main :: IO ()
main = withOpenSSL $ do
	args <- (fmap mountAt . splitAt 5) <$> getArgs
	case args of
		([dbpth,vuser,vkey,vtoken,rAddr], Just (root, port)) -> do
			let vogogo = Vgg.Auth vuser vkey vtoken
			cwd <- getWorkingDirectory
			db <- open dbpth
			run port $
				logStdoutDev $ autohead $ acceptOverride $ jsonp $  -- Middleware
				dispatch (staticRoot cwd) $ routes root db vogogo (read rAddr)
		(_, Nothing) -> hPutStrLn stderr "Invalid Root URI given"
		_ -> hPutStrLn stderr "Usage: ./Main <db path> <Vogogo User> <Vogogo API Key> <Vogogo App Token> [<Root URI>] [<port>]"
