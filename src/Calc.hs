{-# LANGUAGE KindSignatures #-}

module Calc (someFunc) where
import Data.Kind ( Type )
import Text.Read (readMaybe)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

instance Semigroup Int where
  (<>) :: Int -> Int -> Int
  (<>) x y  = x + y

add :: Semigroup a => a -> a -> a 
add n x = n <> x

r :: Int
-- r = add (add (add 2 3) 5) 10
r = 2 `add` 3 `add` 5 `add` 10

r' :: Int
r' = add 10 $ add 5 $ add 2 3

sharedFunc :: Int -> Int
sharedFunc = add 10 . add 5

r'' :: Int
r'' = sharedFunc $ add 2 3

f :: Int -> Int
f = undefined

g :: Int -> Int
g = undefined

r''' :: Int -> Int
r''' = f . g

r'''' :: Int -> Int
r'''' x = f (g x)

v :: String
v = "(((2 add 3) add 5) add 10)"

v' :: String
v' = "2"

v'' :: String
v'' = "3"

v''' :: String
v''' = "5 add 10"

v'''' :: String
v'''' = "2 add 3"

addOne :: [Int] -> [Int]
addOne [] = []
addOne (x : xs) = x +1 : addOne xs

data Expression = 
    Num Int 
  | Add Expression Expression deriving Show

evalExpression :: Expression -> Int
evalExpression (Num x) = x
evalExpression (Add x y) = evalExpression x + evalExpression y

data NewInt = NewInt Int
v''''' :: NewInt
v''''' = NewInt 2

parseInt :: String -> Maybe Int
parseInt = readMaybe

y :: Maybe Int
y = parseInt "3000"

parseAdd :: Semigroup a => String -> Maybe (a -> a -> a)
parseAdd input = do 
  if input == "add"
    then Just add
    else Nothing

-- parseTerm :: String -> 
