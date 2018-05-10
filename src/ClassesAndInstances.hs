module ClassesAndInstances where

import Data.Char
import Data.List

-- |Type classes pattern allow you to define a single type class [ArithmeticTypeClass] and then give it
--  different implementations [instance] depending the type.
--  Once we use that functions defined, it works like implicit in scala, it will use the implementation [instance] for that type used.

-- | Here we just define the class with the structure as a contract.
class ArithmeticTypeClass x where
    customSum :: x -> x -> x
    customMultiply :: x -> x -> x

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

d1 = 6 :: Double
d2 = 7 :: Double

-- |For this two function consumption it will use instance implementation of Double
sumDoubleArithmeticTypeClass = customSum d1 d2
multiplyDoubleArithmeticTypeClass = customMultiply d1 d2


class BasicEq i1 where
    isEqual :: i1 -> i1 -> Bool

instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False

output = isEqual False False

