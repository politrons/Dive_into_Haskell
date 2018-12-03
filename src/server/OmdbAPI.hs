{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module OmdbAPI where
{-| Library to use for multi line string-}
import Text.RawString.QQ
import Web.Scotty
import Data.Monoid ((<>))
import GHC.Generics

import Web.Scotty.Internal.Types (ScottyT, ActionT, Param, RoutePattern, Options, File)
import Control.Concurrent (myThreadId,newEmptyMVar,forkIO,threadDelay,putMVar,takeMVar)
import System.Random (randomRIO)
import Control.Concurrent.Async (async,wait)
import Data.IORef (newIORef,IORef,atomicModifyIORef,readIORef,writeIORef)
import Control.Monad.IO.Class (liftIO)
import Text.Read (lift)
import Network.HTTP.Types.Status
import RestConnector
import Data.Aeson (FromJSON,ToJSON,decode)
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Internal as Inter
import qualified Data.Text.Lazy as LazyText

omdbKey = "af40fe8c" :: String
port = 4000 :: Int

searchMovieQuery :: String -> String
searchMovieQuery movie = "http://www.omdbapi.com/?apikey="++ omdbKey ++ "&t=" ++ movie

{-| Using [scotty] passing [port] and [routes] we define the http server-}
startServer :: IO ()
startServer = do
              print ("Starting Omdb server at port: " ++ show port)
              scotty port routes

routes ::  ScottyM()
routes = do get "/movie/:movie" responseMovie

{-| Thanks to [raw] function we can render a ByteString as Json format perfectly-}
responseMovie :: ActionM ()
responseMovie = do movie <- extractUriParam "movie"
                   liftAndCatchIO $ print ("Finding movie" ++ movie)
                   bsResponse <- liftAndCatchIO $ simpleGetRequest $ searchMovieQuery movie
                   liftAndCatchIO $ print ("Response:" ++ (show bsResponse))
                   raw bsResponse

extractUriParam :: LazyText.Text -> ActionM String
extractUriParam param = Web.Scotty.param param
