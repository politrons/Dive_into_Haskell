module Generics where


data Car = Car String deriving (Show, Eq)
data Truck = Truck String deriving (Show, Eq)

data GenericType
  = G1 Car
  | G2 Truck
  deriving (Show, Eq)

{-| _unique function definition with two implementations, is not the way of doing Generic in haskell, but
    is indeed an easy way. Similar to Type classes but less verbose.-}
genericFeature :: GenericType -> IO()
genericFeature (G1 apple) = print apple
genericFeature (G2 pineapple) = print pineapple

