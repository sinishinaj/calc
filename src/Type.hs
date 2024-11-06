{-# LANGUAGE InstanceSigs #-}
module Type(MyNumber(..)) where

class MyNumber n where
  (!+!) :: n -> n -> n

instance MyNumber Int where
  (!+!) :: Int -> Int -> Int
  (!+!) x y = x + y

instance MyNumber Float where
  (!+!) :: Float -> Float -> Float
  (!+!) x y = x + y

instance MyNumber Bool where
  (!+!) :: Bool -> Bool -> Bool
  (!+!) x y = x || y