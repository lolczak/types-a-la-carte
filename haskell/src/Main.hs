{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Coproduct
import Expr

addExample :: Expr(Val :+: Add)
addExample = In(Inr(Add (In(Inl(Val 118))) (In(Inl(Val 1219)) )))

main = do putStr "addExample: "
          putStr (show (eval addExample))
