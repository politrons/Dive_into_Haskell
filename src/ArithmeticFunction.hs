module ArithmeticFunction where

-- |You have two ways to define functions. With and without lambdas
--arithmeticFunction number = number * 100
--The same than the line below
multiplyFunc = (\number -> number * 100):: Int -> Int

sumFunc = (\number -> number + 1000) :: Int -> Int

subtractFunc = (\number -> \amount -> number - amount ) :: Int -> Int -> Int

-- |combining functions
responseValue = sumFunc (multiplyFunc (multiplyFunc 5))

-- | In this function one value it's coming from previous function sumFunction, and the 100
subtractResponse = subtractFunc (sumFunc 200) 100

-- |Running the expression. Using show operator we transform from Int -> String
numericOutput = putStrLn (show responseValue)

-- |print it will print in screen all types that implement show. Since Int is one of them, we're good!

--numericOutput = print responseValue