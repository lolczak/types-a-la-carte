{-# LANGUAGE TypeOperators #-}

module Expr where

import Data.Functor()
import Coproduct

data Expr f = In( f (Expr f))

data Val e = Val Int

data Add e = Add e e

instance Functor Val where
    fmap f (Val x) = Val x

instance Functor Add where
    fmap f (Add x y) = Add (f x) (f y)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
    evalAlgebra :: f Int -> Int

instance Eval Val where
    evalAlgebra (Val x) = x

instance Eval Add where
    evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl x) = evalAlgebra x
    evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr
