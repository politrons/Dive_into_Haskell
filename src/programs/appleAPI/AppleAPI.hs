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
import Data.Aeson (ToJSON,FromJSON,parseJSON,FromJSON,decode)
import Data.Char (toUpper)

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
                         get "/product/:product" $ responseProduct ioRefManager
                         get "/product/:product/min/:minPrice/max/:maxPrice" $ responseProductByPrice ioRefManager
                         get "/band/:band/album/:album" $ responseBandAndAlbum ioRefManager


{-| We use [text] operator from scotty we render the response in text/plain-}
responseService :: ActionM ()
responseService = text "Apple API Haskell server 1.0"

responseName :: ActionM ()
responseName = text "Pablo Perez Garcia"

responseProduct :: IORef Manager -> ActionM ()
responseProduct ioRefManager = do product <- extractUriParam "product"
                                  liftAndCatchIO $ print ("Finding apple product:" ++ product)
                                  bsResponse <- liftAndCatchIO $ requestToAppleAPI ioRefManager (appleAPI product)
                                  products <- liftAndCatchIO $ decodeJsonToDataType bsResponse
                                  products <- liftAndCatchIO $ setGenreInUpper products
                                  json products

responseBandAndAlbum :: IORef Manager -> ActionM ()
responseBandAndAlbum ioRefManager = do band <- extractUriParam "band"
                                       album <- extractUriParam "album"
                                       liftAndCatchIO $ print ("Finding apple product band " ++ band ++ "and album " ++ album)
                                       bsResponse <- liftAndCatchIO $ requestToAppleAPI ioRefManager (appleAPI band)
                                       products <- liftAndCatchIO $ decodeJsonToDataType bsResponse
                                       filterProducts <- liftAndCatchIO $ filterByAlbum album products
                                       filterProducts <- liftAndCatchIO $ setGenreInUpper filterProducts
                                       json filterProducts

responseProductByPrice :: IORef Manager -> ActionM ()
responseProductByPrice ioRefManager = do product <- extractUriParam "product"
                                         minPriceStr <- extractUriParam "minPrice"
                                         maxPriceStr <- extractUriParam "maxPrice"
                                         minPrice <- liftAndCatchIO $ return $ MinPrice (read minPriceStr)
                                         maxPrice <- liftAndCatchIO $ return $ MaxPrice (read maxPriceStr)
                                         bsResponse <- liftAndCatchIO $ requestToAppleAPI ioRefManager (appleAPI product)
                                         products <- liftAndCatchIO $ decodeJsonToDataType bsResponse
                                         filterProducts <- liftAndCatchIO $ filterByMinAndMaxPrice products minPrice maxPrice
                                         filterProducts <- liftAndCatchIO $ setGenreInUpper filterProducts
                                         json filterProducts

{-| Function to extract uri params by name-}
extractUriParam :: LazyText.Text -> ActionM String
extractUriParam param = Web.Scotty.param param

{-| Function that receive a ByteString in json format and we decode using [decode] operator into a [Maybe T] where
   T is the type that thanks to the the type classes defined  at the bottom of the module can bee inference. -}
decodeJsonToDataType :: ByteString -> IO Products
decodeJsonToDataType json = case decode json of
                                 Just products -> return products
                                 Nothing ->  return emptyProducts

{-| Filter function that using [filter] operator we create a new Products record with a filter list of products by collectionName-}
filterByAlbum :: [Char] -> Products -> IO Products
filterByAlbum album products = return Products { results = filter (\product -> collectionName product == album) (results products) }

filterByMinAndMaxPrice ::Products-> MinPrice -> MaxPrice -> IO Products
filterByMinAndMaxPrice products (MinPrice min) (MaxPrice max) = do let newProducts = filter(\product -> (trackPrice product) > min && (trackPrice product) < max ) (results products)
                                                                   return Products { results = newProducts}

{-| Function to map the list of Products and set [primaryGenreName] in upper case-}
setGenreInUpper :: Products -> IO Products
setGenreInUpper products = return Products {results = map (\product -> product { primaryGenreName = map toUpper $ primaryGenreName product }) (results products) }

{-| ----------------------------------------------}
{-|                 HTTP CLIENTS                 -}
{-| ----------------------------------------------}

{-| Function to make a request to Apple API. We extract first the manager created to open the request since
   it's quite expensive create the manager per request.-}
requestToAppleAPI :: IORef Manager -> String -> IO ByteString
requestToAppleAPI ioRefManager uri = do manager <- liftIO (readIORef ioRefManager)
                                        request <- parseRequest uri
                                        response <- makeRequest manager request
                                        return $ responseBody response

{-| Function that receive the Manager of Http connection, a Request and using [httpLbs] function we make the request,
    Receiving a [Response] with [ByteString] as T of the body response -}
makeRequest :: Manager -> Request -> IO (Response ByteString)
makeRequest manager request =  httpLbs request manager


{-| ----------------------------------------------}
{-|                    MODEL                     -}
{-| ----------------------------------------------}
data Products = Products { results :: [Product]} deriving (Show, Generic)

data Product = Product {
     artistName:: [Char],
     trackName:: [Char],
     collectionName:: [Char],
     primaryGenreName:: [Char],
     trackPrice:: Double,
     trackViewUrl:: [Char],
     releaseDate:: [Char],
     previewUrl:: [Char],
     artworkUrl100:: [Char]
} deriving (Show, Generic)

{-| We define Aeson instance of TypeClass ToJSON to serialize and FromJSON to deserialize-}
instance ToJSON Products
instance FromJSON Products
instance ToJSON Product
instance FromJSON Product

data MinPrice = MinPrice Double
data MaxPrice = MaxPrice Double

emptyProducts = Products [Product {
               artistName="",
               trackName="",
               collectionName="",
               primaryGenreName="",
               trackPrice=0.0,
               trackViewUrl="",
               releaseDate="",
               previewUrl="",
               artworkUrl100=""
               }] :: Products
