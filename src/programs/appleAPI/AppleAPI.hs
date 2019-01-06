{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
{-# LANGUAGE DeriveGeneric #-}
module AppleAPI where

--Server
import Web.Scotty
import Data.Monoid ((<>))
import GHC.Generics
import Web.Scotty.Internal.Types (ScottyT, ActionT, Param, RoutePattern, Options, File)
import Web.Scotty (ScottyM,scotty,ActionM,get,text)
import qualified Data.Text.Lazy as LazyText
--Http client
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString (pack,unpack)
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Exception (SomeException,try,evaluate)
import Network.HTTP.Client (Response)
import Data.IORef (newIORef,IORef,readIORef)
import Control.Monad.IO.Class (liftIO)

appleAPI :: [Char] -> String
appleAPI entry = "http://itunes.apple.com/search?term=" ++ entry

servicePort = 5000 :: Int

{-| ----------------------------------------------}
{-|                    SERVER                    -}
{-| ----------------------------------------------}
{-| Using [scotty] passing [port] and [routes] we define the http server-}
appleServer :: IO ()
appleServer = do
    print ("Starting Apple Server at port " ++ show servicePort)
    manager <- newManager defaultManagerSettings
    ioRefManager <- newIORef manager
    scotty servicePort (routes ioRefManager)

{-| We define the routes thanks to REST operators [get, post, put, delete, patch] which expect to
    receive a [RoutePattern] as a path and a [ActionM] as the action of the request. Then we return a [ScottyM]-}
routes :: IORef Manager -> ScottyM()
routes ioRefManager = do get "/service" responseService
                         get "/author" responseName
                         get "/entry/:entry" $ responseProduct ioRefManager

{-| We use [text] operator from scotty we render the response in text/plain-}
responseService :: ActionM ()
responseService = text "Apple API Haskell server 1.0"

responseName :: ActionM ()
responseName = text "Pablo Perez Garcia"


{-| Thanks to [raw] function we can render a ByteString as Json format perfectly-}
responseProduct :: IORef Manager -> ActionM ()
responseProduct ioRefManager = do entry <- extractUriParam "entry"
                                  liftAndCatchIO $ print ("Finding apple product" ++ entry)
                                  bsResponse <- liftAndCatchIO $ simpleGetRequest ioRefManager (appleAPI entry)
                                  raw bsResponse

extractUriParam :: LazyText.Text -> ActionM String
extractUriParam param = Web.Scotty.param param

{-| ----------------------------------------------}
{-|                 HTTP CLIENTS                 -}
{-| ----------------------------------------------}

{-| Function to make a request to Apple API. We extract first the manager created to open the request since
   it's quite expensive creare the manager per request.
-}
simpleGetRequest :: IORef Manager -> String -> IO ByteString
simpleGetRequest ioRefManager uri = do manager <- liftIO (readIORef ioRefManager)
                                       request <- parseRequest uri
                                       response <- makeRequest manager request
                                       return $ responseBody response

makeRequest :: Manager -> Request -> IO (Response ByteString)
makeRequest manager request =  httpLbs request manager