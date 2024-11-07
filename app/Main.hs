module Main where

import Lang (Expression(..), evalExpression)
import Control.Applicative ( (<|>) )
import Parser (parseExpression)

toResult :: Maybe (String, Expression Float) -> Maybe (Expression Float)
toResult (Just (_, expression)) = Just expression
toResult Nothing = Nothing

data CalcNumber = CalcInt Int | CalcBool Bool | CalcFloat Float deriving Show
calc :: String -> Maybe CalcNumber
calc input = calcBool <|> calcInt <|> calcFloat
  where
    calcFloat :: Maybe CalcNumber
    calcFloat = do
      (_, e) <- parseExpression input :: Maybe (String, Expression Float)
      let result = evalExpression e
      Just (CalcFloat result)
    calcInt :: Maybe CalcNumber
    calcInt = do
      (_, e) <- parseExpression input :: Maybe (String, Expression Int)
      let result = evalExpression e
      Just (CalcInt result)
    calcBool :: Maybe CalcNumber
    calcBool = do
      (_, e) <- parseExpression input :: Maybe (String, Expression Bool)
      let result = evalExpression e
      Just (CalcBool result)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let expression = Num 2 `Add` Num 2 `Add` Num 4 `Add` Num 4 :: Expression Int
  let shortExpression = Num 1 :: Expression Int
  print expression
  print shortExpression
  let result = evalExpression expression
  let shortResult = evalExpression shortExpression
  print result
  print shortResult
  --let input = "2 add 4 add 6 add 8 add 10"
  let input = "0.0 add 1 add 2 add 0.0 add 1.5"
  let mExpression = toResult $ parseExpression input
  case mExpression of 
    Nothing -> print "Error."
    Just exp' -> print $ evalExpression exp'
