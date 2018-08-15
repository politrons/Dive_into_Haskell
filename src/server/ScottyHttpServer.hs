{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
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

port = 3000 :: Int

{-| Thanks to type class we define that any [User] is JSON serializable/deserializable.|-}
instance ToJSON User
instance FromJSON User

{-| Using [scotty] passing [port] and [routes] we define the http server-}
scottyServer :: IO ()
scottyServer = do
    print ("Starting Server at port " ++ show port)
    scotty port routes

{-| We define the routes thanks to REST operators [get, post, put, delete, patch] which expect to
    receive a [RoutePattern] as a path and a [ActionM] as the action of the request. Then we return a [ScottyM]-}
routes :: ScottyM()
routes = do get "/service" responseService
            get "/name" responseName
            get "/hello/:name" responseHello -- Using :val we can set the variable name to be candidate to be extracted
            get "/users" responseUsers
            get "/users/:id" responseUserById
            get "/user/:name" responseUserByName
            post "/user/" createUser
            put "/user/" updateUser
            delete "/users/:id" deleteById

{-| We use [text] operator from scotty we render the response in text/plain-}
responseService :: ActionM ()
responseService = text "First Haskell service 1.0"

responseName :: ActionM ()
responseName = text "Paul Perez Garcia"

responseHello :: ActionM ()
responseHello = do name <- param "name" -- using [param] operator we can extract the uri param
                   text ("hello " <> name <> "!")

{-| Thanks to Aeson library and encode, we can use [json] operator to allow us to encode object into json.|-}
responseUsers :: ActionM ()
responseUsers = json allUsers

responseUserByName :: ActionM ()
responseUserByName = do name <- param "name"
                        if areEquals name "Paul" then json paul
                        else if areEquals name "John" then json john
                        else text "Do I know you?"

{-| In scotty we have [param] operator which used passing the uri param name we can extract the value. -}
responseUserById :: ActionM ()
responseUserById = do id <- param "id"
                      user <- liftAndCatchIO $ getUserById id
                      json user

{-| This part of the program is really interested, we are using function where first we need to call insertUser
    passing a [User] but we have a [Maybe User] so we use a functor [<*>] to extract the User from the Maybe.
     Then we have [sequence] operator which does:
    -- | Evaluate each monadic action in the structure from left to right, and collect the results.
    Then finally we need to lift the response from insertUser  [IO OK] to [OK] and to do that we use
    the operator [liftAndCatchIO] which does:
    -- | Like 'liftIO', but catch any IO exceptions and turn them into Scotty exceptions.
-}
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

----------------------------------------------------------------------------------------------------------------------
hasId :: Int -> User -> Bool
hasId id user = userId user == id

areEquals :: String -> String -> Bool
areEquals requestName name = requestName == name

paul :: User
paul = User { userId = 1, userName = "Paul" }

john :: User
john = User { userId = 2, userName = "John" }

allUsers :: [User]
allUsers = [paul, john]