module ClassesAndInstances where

import Data.Char
import Data.List
import Data.String

-- |Type classes pattern allow you to define a single type class [ArithmeticTypeClass] and then give it
--  different implementations [instance type] depending the type.
--  Once we use that functions defined, it works like implicit in scala, it will use the implementation [instance] for that type used.

-- | Here we just define the class with the structure as a contract.
class ArithmeticTypeClass _type where
    customSum :: _type -> _type -> _type
    customMultiply :: _type -> _type -> _type

-- |Here we define the implementation for type Integer.
instance ArithmeticTypeClass Integer where
        customSum i1 i2 = i1 + i2
        customMultiply i1 i2 = i1 * i2

-- |Here we define the implementation for type Double.
instance ArithmeticTypeClass Double where
        customSum d1 d2 = d1 + d2
        customMultiply d1 d2 = d1 * d2

i1 = 6 :: Integer
i2 = 7 :: Integer

-- |For this two function consumption it will use instance implementation of Integer
sumIntArithmeticTypeClass = customSum i1 i2
multiplyIntArithmeticTypeClass = customMultiply i1 i2

d1 = 7 :: Double
d2 = 7 :: Double

-- |For this two function consumption it will use instance implementation of Double
sumDoubleArithmeticTypeClass = customSum d1 d2
multiplyDoubleArithmeticTypeClass = customMultiply d1 d2

-- |Here we can define an equals type class, where we define that we have a function that receive two arguments
--  and return a Boolean
class MyEquals i1 where
    isEqual :: i1 -> i1 -> Bool

-- |Here we create an implementation to compare Booleans
instance MyEquals Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False

-- |Here we create an implementation to compare Integer
instance MyEquals Integer where
    isEqual a  b  = a == b

-- |Here we create an implementation to compare Double
instance MyEquals Double where
    isEqual a  b  = a == b

output = isEqual False False
compareIntOutput = isEqual i1 i2
compareDoubleOutput = isEqual d1 d2


