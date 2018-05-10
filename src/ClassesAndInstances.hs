module ClassesAndInstances where


class MyFirstClass x where

    fromInt :: Int -> x
    fromString :: String -> x

class  Eq a  where
   (==), (/=) :: a -> a -> Bool
--
--   x /= y     =  not (x == y)
--   x == y     =  not (x /= y)

