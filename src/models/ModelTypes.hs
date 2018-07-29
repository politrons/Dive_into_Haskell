{-# LANGUAGE OverloadedStrings #-} -- Mandatory language overload to overload String
{-# LANGUAGE DeriveGeneric #-}
module ModelTypes where

import GHC.Generics

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)

getUserId :: User -> Int
getUserId(User userId _) = userId

getUserName :: User -> String
getUserName(User _ userName) = userName