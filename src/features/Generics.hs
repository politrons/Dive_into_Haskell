module Generics where

--import ModelTypes

type Name = String

type Age = Int

data GenericType = Age | Name deriving (Show)

class Types t where
    genericEchoFunction:: t -> t

instance Types GenericType  where
    genericEchoFunction age =  age


--genericFeature :: GenericType -> IO()
--genericFeature = do user <- return $ genericEchoFunction "Paul"
--                    print user

