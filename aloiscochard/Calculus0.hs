module Calculus0 where

data Exp
  = Lit Int
  | App Op Exp Exp

data Op
 = Add
 | Sub

-- | 21 + 21
simpleExp :: Exp
simpleExp = App Add (Lit 21) (Lit 21)

runExp :: Exp -> Int
runExp (Lit x) = x
runExp (App Add lhs rhs) =
  (runExp lhs) + (runExp rhs)
runExp (App Sub lhs rhs) =
  (runExp lhs) - (runExp rhs)

run :: Int
run = runExp simpleExp
