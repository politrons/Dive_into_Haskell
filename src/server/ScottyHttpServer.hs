{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
module ScottyHttpServer where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON, encode,decode)
import GHC.Generics
import ModelTypes
import MySQLConnector

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
            delete "/users/:id" deleteUserById

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
                      json (filter (hasId id) allUsers)

createUser :: ActionM ()
createUser =  do
                 user <- getUserParam
                 -- Persist the user
                 json user

updateUser :: ActionM ()
updateUser =  do user <- getUserParam
                 -- Update the user
                 json user

deleteUserById :: ActionM ()
deleteUserById = do id <- param "id"
                    -- Delete user
                    json (filter (hasId id) allUsers)

{-| In scotty we have [body] operator to get the request body.
    We also use [decode] operator to extract and transform from json to Maybe of type we specify in the type signature-}
getUserParam = do requestBody <- body
                  return (decode requestBody :: Maybe User)

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