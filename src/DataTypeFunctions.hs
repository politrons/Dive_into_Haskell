module DataTypeFunctions where

import Data.Char
import Data.List

-- | Haskell is a Strong type language and it encourage of the use of types all the time.
--  It make it really simple create new types just using the operator [type] or algebras using [data]

-- |Data type
--  ---------

-- | We specify to Haskell a new data type Fruit, which can only be a type Apple, Banana, Pineapple and Orange
data Fruit = Apple
          | Banana
          | Pineapple
          | Orange
          | Coconut
          | Strawberry

-- | We define a function which receive a Fruit type and return the description of the ingredients
menuFunc :: Fruit -> String
menuFunc fruit =  case fruit of
                       Apple   -> "Sweet, red and hard"
                       Banana  -> "Sweet, yellow and soft"
                       Pineapple   -> "Bitter, yellow and soft"
                       Orange      -> "Bitter, orange and soft"
                       Coconut      -> "Sweet, brown and hard"
                       Strawberry      -> "Bitter, red and soft"

type Smoothie = [Fruit]
tropicalSmoothie = [Pineapple, Banana, Coconut] :: Smoothie

outputMenu =  map menuFunc tropicalSmoothie

-- | Product Type
--   ------------
-- | Types defined form haskell types
type Name = String
type Surname = String
type Age = Int

-- | Algebras defined as sum type and product type
--  Sum types are those types like enum where use polymorphism and only can have one specific type. In This case
--  Male or Female
data Sex = Male | Female -- Data type or Sum type

-- | Product types are a different
-- | For a reason I don't understand when we define products we have to add the product type as first element
data Person = Person Name Surname Age Sex -- Data product

spanish1 = Person "Pablo" "Perez"  37  Male :: Person
spanish2 = Person "Jorge" "Gonzales"  40 Male :: Person
spanish3 = Person "Susana" "Garcia" 21 Female :: Person

spanishPeople = [spanish1, spanish2, spanish3] :: [Person]

-- | We create human type for output since data product cannot be processed by IO print
type Human = (Name, Surname, Age)

-- | Pipelines

outputOlderThan30 = map (\person -> map toUpper (getName person)) $
                    filter (\person -> getAge person > 30) spanishPeople

womenOutput = map transformPersonFunc $
              filter (\person -> isWomenFunc person) spanishPeople


-- | Getters
getAge :: Person -> Age
getAge(Person _ _ age _)= age

getName :: Person -> Name
getName(Person name _ _ _) = name

getSurname :: Person -> Surname
getSurname(Person _ surname _ _ ) = surname

getSex :: Person -> Sex
getSex(Person _ _ _ sex) = sex

-- | Utils functions
transformPersonFunc = (\person -> (toUpperFunc (getName person),
                                   toUpperFunc (getSurname person),
                                   getAge person + 100)) :: Person -> Human

isWomenFunc :: Person -> Bool
isWomenFunc person = case (getSex person) of
             Male   -> False
             Female  -> True

toUpperFunc :: String -> String
toUpperFunc = \value ->  map toUpper value
