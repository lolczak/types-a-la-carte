{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Coproduct
import Expr

addExample :: Expr(Val :+: Add)
addExample = In(Inr(Add (In(Inl(Val 118))) (In(Inl(Val 1219)) )))

main :: IO ()
main = do putStrLn $ "addExample: " ++ (show $ eval addExample)
          let x :: Expr (Add :+: Val) = val 30000 |+| val 1330 |+| val 7
          putStrLn $ "x: " ++ (show $ eval x)

