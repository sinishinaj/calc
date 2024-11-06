{-# LANGUAGE InstanceSigs #-}
module Parser where
import Lang (Expression(..))
import Data.Maybe ( isJust )

s' :: String
s' = "2 add 3 add 4 add 5"

s'' :: String
s'' = "2 add 3"

s''' :: String
s''' = "2"

readCharacter :: String -> Maybe (String, Char)
readCharacter "" = Nothing
readCharacter (c : input) = Just (input, c)

isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _ = False

parseDigit :: String -> Maybe (String, Char)
parseDigit input = do
  (input', c) <- readCharacter input
  if isDigit c
    then Just (input', c)
    else Nothing

extract :: [(String, Char)] -> (String, String)
extract pairs = (extractRemaining pairs, extractChars pairs)
  where
    extractRemaining :: [(String, Char)] -> String
    extractRemaining [] = ""
    extractRemaining pairs' = fst (last pairs')
    extractChars :: [(String, Char)] -> String
    extractChars [] = ""
    extractChars (pair' : pairs') = snd pair' : extractChars pairs'

parseDigits :: String -> Maybe (String, String)
parseDigits input = do
  pairs <- sequence (parseDigits' input)
  toResult (extract pairs)
  where
    toResult :: (String, String) -> Maybe (String, String)
    toResult ("", "") = Nothing
    toResult p = Just p 
    parseDigits' :: String -> [Maybe (String, Char)]
    parseDigits' "" = []
    parseDigits' i = do
      let x = parseDigit i
      if isJust x
        then x : parseDigits' (tail i)
        else []

skipSpace :: String -> Maybe (String, String)
skipSpace input = do
  pairs <- sequence $ space' input
  Just $ extract pairs
  where
    space' :: String -> [Maybe (String, Char)]
    space' "" = []
    space' (c : cs) = if c == ' '
      then space' cs
      else [Just (c : cs, ' ')]

class NumberParser a where
  parseNumber :: String -> Maybe (String, Expression a)

instance NumberParser Int where
  parseNumber :: String -> Maybe (String, Expression Int)
  parseNumber input = do
    (input', _) <- skipSpace input
    (input'', digits) <- parseDigits input'
    Just (input'', toNumber digits)
      where
        toNumber :: String -> Expression Int
        toNumber s = do
          let v = read s :: Int
          Num v

instance NumberParser Float where
  parseNumber :: String -> Maybe (String, Expression Float)
  parseNumber input = do
    (input', _) <- skipSpace input
    (input'', digits) <- parseDigits input'
    Just (input'', toNumber digits)
      where
        toNumber :: String -> Expression Float
        toNumber s = do
          let v = read s :: Float
          Num v

instance NumberParser Bool where
  parseNumber :: String -> Maybe (String, Expression Bool)
  parseNumber input = do
    (input', _) <- skipSpace input
    (input'', digits) <- parseDigits input'
    Just (input'', toNumber digits)
      where
        toNumber :: String -> Expression Bool
        toNumber "0" = Num False
        toNumber "1" = Num True
        toNumber _ = undefined

parseChar :: String -> Char -> Maybe (String, Char)
parseChar input c = do
  (input', c') <- readCharacter input
  if c' == c
    then Just (input', c')
    else Nothing

parseString :: String -> String -> Maybe (String, String)
parseString input str = do
  pairs <- sequence $ parseString' input str
  Just $ extract pairs
  where
    parseString' :: String -> String -> [Maybe (String, Char)]
    parseString' _ "" = []
    parseString' i (c : cs) = parseChar i c : parseString' (tail i) cs

parseSimpleExpression :: (NumberParser a) => String -> Maybe (String, Expression a)
parseSimpleExpression input = do
  (input',_) <- skipSpace input
  (input'', leftNumber) <- parseNumber input'
  (input''', _) <- skipSpace input''
  (input'''', _) <- parseString input''' "add"
  (input''''', _) <- skipSpace input''''
  (input'''''', rightNumber) <- parseNumber input'''''
  Just(input'''''', leftNumber `Add` rightNumber)

parseLeftSimpleExpression :: (NumberParser a) => String -> Maybe (String, Expression a)
parseLeftSimpleExpression input = do
  (input',_) <- skipSpace input
  (input'', leftNumber) <- parseNumber input'
  (input''', _) <- skipSpace input''
  (input'''', _) <- parseString input''' "add"
  (input''''', _) <- skipSpace input''''
  (input'''''', rightExpression) <- parseExpression input'''''
  Just(input'''''', leftNumber `Add` rightExpression)

parseExpression :: (NumberParser a) => String -> Maybe (String, Expression a)
parseExpression input = do
  r <- finalParse parseLeftSimpleExpression input
  undefined
  -- | isJust $ parseLeftSimpleExpression input = finalParse parseLeftSimpleExpression input
  -- | isJust $ parseSimpleExpression input = finalParse parseSimpleExpression input
  -- | isJust $ parseNumber input = finalParse parseNumber input
  -- | otherwise = Nothing

finalParse :: (String -> Maybe (String, Expression a)) -> String -> Maybe (String, Expression a)
finalParse parseFunc input = do
  (input', expression') <- parseFunc input
  (input'', _) <- skipSpace input'
  _ <- eOf input''
  Just("", expression')
  where
    eOf :: String -> Maybe ()
    eOf "" = Just ()
    eOf _  = Nothing

e = "1  add  2  add 3"