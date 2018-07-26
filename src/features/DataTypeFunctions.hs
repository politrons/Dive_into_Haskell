module DataTypeFunctions where

import Data.Char
import Data.List

-- | Haskell is a Strong type language and it encourage of the use of types all the time.
--  It make it really simple create new types alias just using the operator [type] or product types (algebras) using [data]

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
--  Male or Female.
--  Also the operator deriving (Show) it will allow make the algebra being extendable from print
data Sex = Male | Female deriving (Show) -- Data type or Sum type

-- | Product types are a different
-- | We also have to define as first argument the Person again, as value constructor name.
data Person = Person{
                      name::Name,
                      surname :: Surname,
                      age::Age,
                      sex::Sex
                    } deriving (Show) -- Data product

spanish1 = Person {
                    name="Pablo",
                    surname="Perez",
                    age=37,
                    sex=Male
                   } :: Person

spanish2 = Person "Jorge" "Gonzales"  40 Male :: Person
spanish3 = Person "Susana" "Garcia" 21 Female :: Person

spanishPeople = [spanish1, spanish2, spanish3] :: [Person]

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

outputPerson = Person (toUpperFunc "") "Perez"  37  Male :: Person


-- | Utils functions
transformPersonFunc :: Person -> Person
transformPersonFunc  = \person -> Person (toUpperFunc (getName person))
                                   (toUpperFunc (getSurname person))
                                   (getAge person + 100)
                                    (getSex person)
-- Predicate function
isWomenFunc :: Person -> Bool
isWomenFunc person = case (getSex person) of
             Male   -> False
             Female  -> True

toUpperFunc :: String -> String
toUpperFunc = \value ->  map toUpper value
