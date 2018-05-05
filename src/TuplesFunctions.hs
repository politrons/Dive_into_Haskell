module TuplesFunctions (sorterNameOutput,olderOutput) where

import Data.Char
import Data.List

-- | Haskell is a strong type language and just like in Scala we can define better business logic types
--   from primitives using [type]

type Name = String
type SurName = String
type Age = Int
type Person = (Name, SurName, Age)

-- | We create our instance of Person  passing the values defined in Person type
person1 = ("Paul", "Perez", 37) :: Person
person2 = ("John", "Mcklein", 40) :: Person
person3 = ("Arnold", "Swache", 50) :: Person

personList = [person1, person2, person3] :: [Person]

-- | To extract a value from the Person instance we need to create a function that select the element from the tuple
getName :: Person -> Name
getName(name,_,_) = name

getSurname :: Person -> SurName
getSurname(_,surname,_) = surname

getAge :: Person -> Age
getAge(_,_,age) = age

-- | Transform/filter Functions

isNameLengthFunc =  (\name ->  length name == 4) :: Name -> Bool

isOlderAgeThanFunc = (\person -> (getAge person) >= age ) :: Person -> Bool

upperCaseFunc = (\person -> ((map toUpper (getName person)),
                                           getSurname person,
                                           getAge person )) :: Person -> Person

-- | Pipelines

sorterNameOutput = filter isNameLengthFunc  $
                   map getName personList

age = 40

olderOutput = map upperCaseFunc $
              filter isOlderAgeThanFunc personList



