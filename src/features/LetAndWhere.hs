module LetAndWhere where

{-| The scope of the declarations is the expression and the right hand side of the declarations.
    Also if the declaration expect to receive an input value, it's consider as a high order function.
    [let] declaration is lazy by default and only when is invoked is when the declaration pass from
    lazy to eager.
-}
letDeclaration :: IO()
letDeclaration = do
              let letFunc z= 1+2+z
              value <- invokeLetDeclaration letFunc
              print value


invokeLetDeclaration :: (Integer -> Integer) -> IO Integer
invokeLetDeclaration letFunc =  return $ (letFunc 3)