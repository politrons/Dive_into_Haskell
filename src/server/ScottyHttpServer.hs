{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String

{-| This http server has been build in top of open source library http://hackage.haskell.org/package/scotty-}
module ScottyHttpServer where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON, encode,decode)
import GHC.Generics
import ModelTypes
import MySQLConnector

import Data.ByteString.Lazy.Char8 (ByteString)
import Web.Scotty.Internal.Types (ScottyT, ActionT, Param, RoutePattern, Options, File)
import Data.Text.Lazy (Text)
import Control.Concurrent (myThreadId,newEmptyMVar,forkIO,threadDelay,putMVar,takeMVar)
import System.Random (randomRIO)

port = 3000 :: Int

{-| Thanks to type class we define that any [User] is JSON serializable/deserializable.|-}
instance ToJSON User
instance FromJSON User
instance ToJSON Profile
instance FromJSON Profile
instance ToJSON Address
instance FromJSON Address

{-| Using [scotty] passing [port] and [routes] we define the http server-}
scottyServer :: IO ()
scottyServer = do
    print ("Starting Server at port " ++ show port)
    scotty port routes

{-| We define the routes thanks to REST operators [get, post, put, delete, patch] which expect to
    receive a [RoutePattern] as a path and a [ActionM] as the action of the request. Then we return a [ScottyM]-}
routes :: ScottyM()
routes = do get "/service" responseService
            get "/author" responseName
            get "/users" responseUsers
            get "/user/id/:id" responseUserById
            get "/user/name/:name" responseUserByName
            post "/user/" createUser
            put "/user/" updateUser
            delete "/users/:id" deleteById
            post "/profile/" createProfile


{-| We use [text] operator from scotty we render the response in text/plain-}
responseService :: ActionM ()
responseService = text "First Haskell service 1.0"

responseName :: ActionM ()
responseName = text "Paul Perez Garcia"

{-|  -[Aeson] library and encode operator, we can use [json] operator to allow us to encode object into json.
    - [liftAndCatchIO] operator is used to extract from the IO monad the type and add it to ActionM monad.
    - [forkIO] operator allow use run a do block in a green thread allowing not block the OS threads for transport layer.
    |-}
createProfile :: ActionM ()
createProfile =  do
                 maybeProfile <- getProfileParam
                 user <- return (getUserFromMaybeProfile maybeProfile)
                 address <- return (getAddressFromMaybeProfile maybeProfile)
                 emptyUserVar <- liftAndCatchIO $ newEmptyMVar
                 liftAndCatchIO $ forkIO $ do
                                          status <- insertUser user
                                          putMVar emptyUserVar status
                 emptyAddressVar <- liftAndCatchIO $ newEmptyMVar
                 liftAndCatchIO $ forkIO $ do
                                           status <- insertAddress address
                                           putMVar emptyAddressVar status
                 userStatus <- liftAndCatchIO $ takeMVar emptyUserVar
                 addressStatus <- liftAndCatchIO $ takeMVar emptyAddressVar
                 json (show userStatus)


responseUsers :: ActionM ()
responseUsers = do users <- liftAndCatchIO getAllUsers
                   json (show users)

responseUserByName :: ActionM ()
responseUserByName = do
                        name <- param "name"
                        user <- liftAndCatchIO $ getUserByUserName name
                        json user

{-| In scotty we have [param] operator which used passing the uri param name we can extract the value. -}
responseUserById :: ActionM ()
responseUserById = do
                      id <- param "id"
                      user <- liftAndCatchIO $ getUserById id
                      json user

{-| This part of the program is really interested, we are using function where first we need to call insertUser
    passing a [User] but we have a [Maybe User] so we use a functor [<*>] to extract the User from the Maybe.
     Then we have [sequence] operator which does:
    -- | Evaluate each monadic action in the structure from left to right, and collect the results.
        Then finally we need to lift the response from insertUser  [IO OK] to [OK] and to do that we use
        the operator [liftAndCatchIO] which does:
    -- | Like 'liftIO', but catch any IO exceptions and turn them into Scotty exceptions.-}
createUser :: ActionM ()
createUser =  do maybeUser <- getUserParam
                 status <- liftAndCatchIO $ sequence $ insertUser <$> maybeUser
                 json (show status)

updateUser :: ActionM ()
updateUser =  do maybeUser <- getUserParam
                 status <- liftAndCatchIO $ sequence $ updateUserById <$> maybeUser
                 json (show status)

deleteById :: ActionM ()
deleteById = do id <- param "id"
                status <- liftAndCatchIO $ deleteUserById id
                json (show status)

{-| In scotty we have [body] operator to get the request body.
    We also use [decode] operator to extract and transform from json to Maybe of type we specify in the type signature-}
getUserParam :: ActionT Text IO (Maybe User)
getUserParam = do requestBody <- body
                  return (decode requestBody)

getProfileParam :: ActionT Text IO (Maybe Profile)
getProfileParam = do requestBody <- body
                     return (decode requestBody)

