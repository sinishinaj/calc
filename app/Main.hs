module Main where

import Lang (Expression(..), evalExpression, evalFloatExpression)
import Parser (parseExpression)

toResult :: Maybe (String, Expression) -> Maybe Expression
toResult (Just (_, expression)) = Just expression
toResult Nothing = Nothing

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let expression = Num 2 `Add` Num 2 `Add` Num 4 `Add` Num 4
  let shortExpression = Num 1
  print expression
  print shortExpression
  let result = evalExpression expression
  let shortResult = evalExpression shortExpression
  print result
  print shortResult
  let input = "2 add 4 add 6 add 8 add 10"
  let mExpression = toResult $ parseExpression input
  case mExpression of 
    Nothing -> print "Error."
    Just exp' -> print $ evalExpression exp'
  let floatExpression = evalFloatExpression AFloat 0.5 `Add` AFloat 1.5
  print floatExpression
