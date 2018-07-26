{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
module ScottyHttpServer where

import Web.Scotty

port = 3000 :: Int

scottyServer = do
    print ("Starting Server at port " ++ show port)
    scotty port routes

routes = do get "/service" responseService
            get "/name" responseName

responseService :: ActionM ()
responseService = text "First Haskell service 1.0"

responseName :: ActionM ()
responseName = text "Paul Perez Garcia"