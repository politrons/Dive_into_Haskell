{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
{-# LANGUAGE DeriveGeneric #-}
module ModelTypes where

import GHC.Generics

data Account = Account {user::User, address::Address}

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)

data Address = Address { id :: Int, number::Int, street::String} deriving (Show, Generic)

getUser :: Account -> User
getUser(Account user _) = user

getAddress :: Account -> Address
getAddress(Account _ address) = address

getUserId :: User -> Int
getUserId(User userId _) = userId

getUserName :: User -> String
getUserName(User _ userName) = userName

