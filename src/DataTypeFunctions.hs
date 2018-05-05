module DataTypeFunctions where

-- | Haskell is a Strong type language and it encourage of the use of types all the time.
--  It make it really simple create new types just using the operator [data]

-- | We specify to Haskell a new type Fruit, which can only be a type Apple, Banana, Pineapple and Orange
data Fruit = Apple
          | Banana
          | Pineapple
          | Orange
          | Coconut
          | Strawberry

-- | We define a function which recieve a Fruit type and return the description of the ingredients
menuFunc :: Fruit -> String
menuFunc fruit =
         case fruit of
             Apple   -> "Sweet, red and hard"
             Banana  -> "Sweet, yellow and soft"
             Pineapple   -> "Bitter, yellow and soft"
             Orange      -> "Bitter, orange and soft"
             Coconut      -> "Sweet, brown and hard"
             Strawberry      -> "Bitter, red and soft"

type Smoothie = [Fruit]
tropicalSmoothie = [Pineapple, Banana, Coconut] :: Smoothie

outputMenu =  map menuFunc tropicalSmoothie
