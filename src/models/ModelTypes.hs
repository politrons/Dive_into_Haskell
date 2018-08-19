{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
{-# LANGUAGE DeriveGeneric #-}
module ModelTypes where

import GHC.Generics

{-| Thanks to the deriving Generic we can use this data as structure to serialize/deserialize from/into json by Aeson-}
data Profile = Profile {user::User, address::Address} deriving (Show, Generic)

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)

data Address = Address { id :: Int, number::Int, street::String} deriving (Show, Generic)

getUserFromMaybeProfile :: Maybe Profile -> User
getUserFromMaybeProfile profile =  case profile of
                            Just profile -> getUser profile
                            Nothing -> User 1 "default"

getAddressFromMaybeProfile :: Maybe Profile -> Address
getAddressFromMaybeProfile profile =  case profile of
                            Just profile -> getAddress profile
                            Nothing -> Address 1 1 "default"
getUser :: Profile -> User
getUser(Profile user _) = user

getAddress :: Profile -> Address
getAddress(Profile _ address) = address

getUserId :: User -> Int
getUserId(User userId _) = userId

getUserName :: User -> String
getUserName(User _ userName) = userName

getAddressId :: Address -> Int
getAddressId(Address id _ _) = id

getAddressNumber :: Address -> Int
getAddressNumber(Address _ number _) = number

getAddressStreet :: Address -> String
getAddressStreet(Address _ _ street) = street