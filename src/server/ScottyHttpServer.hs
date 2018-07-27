{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
{-# LANGUAGE DeriveGeneric #-}
module ScottyHttpServer where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON, encode)
import GHC.Generics

port = 3000 :: Int

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)

{-| Thanks to type class we define that any [User] is JSON serializable/deserializable.|-}
instance ToJSON User
instance FromJSON User

{-| Using [scotty] passing [port] and [routes] we define the http server-}
scottyServer :: IO ()
scottyServer = do
    print ("Starting Server at port " ++ show port)
    scotty port routes

routes :: ScottyM()
routes = do get "/service" responseService
            get "/name" responseName
            get "/hello/:name" responseHello -- Using :val we can set the variable name to be candidate to be extracted
            get "/users" responseUsers
            get "/user/:name" responseUser

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

responseUser :: ActionM ()
responseUser = do name <- param "name"
                  if isPaul name then json paul
                  else if isJohn name then json john
                  else text "Do I know you?"

isPaul = (== "Paul") :: String -> Bool
isJohn = (== "John") :: String -> Bool

----------------------------------------------------------------------------------------------------------------------

paul :: User
paul = User { userId = 1, userName = "Paul" }

john :: User
john = User { userId = 2, userName = "John" }

allUsers :: [User]
allUsers = [paul, john]