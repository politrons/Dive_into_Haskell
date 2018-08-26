{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
{-# LANGUAGE DeriveGeneric #-}
module ModelTypes where

import GHC.Generics
import Data.Int (Int32)

{-| Thanks to the deriving Generic we can use this data as structure to serialize/deserialize from/into json by Aeson-}
data Profile = Profile {user::User, address::Address} deriving (Show, Generic)

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)

data Address = Address { id :: Int, number::Int, street::String} deriving (Show, Generic)

data Username = Username String

data AddressId = AddressId Int

data UserId =  UserId Int

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

toUserName :: String -> Username
toUserName name = Username name

fromUserName :: Username -> String
fromUserName(Username name) = name


toAddressId :: Int -> AddressId
toAddressId id = AddressId id

fromAddressId :: AddressId -> Int
fromAddressId(AddressId id) = id


toUserId :: Int -> UserId
toUserId id = UserId id

fromUserId :: UserId -> Int
fromUserId(UserId id) = id

{-| Transform from Int to Int32 format-}
intToInt32 :: Int -> Int32
intToInt32 userId = fromIntegral (userId :: Int) :: Int32

int32ToInt :: Int32 -> Int
int32ToInt userId = fromIntegral (userId :: Int32) :: Int