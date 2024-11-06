{-# LANGUAGE KindSignatures #-}
module Lang (Expression(..), evalExpression) where
import Type(MyNumber(..))

data Expression a = 
    Num a
  | Add (Expression a) (Expression a) deriving Show

evalExpression :: (MyNumber a) => Expression a -> a
evalExpression (Num x) = x
evalExpression (Add x y) = evalExpression x !+! evalExpression y
