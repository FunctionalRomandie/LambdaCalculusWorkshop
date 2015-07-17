module Calculus1 where

data Exp
  = Lit Int
  | App Op Exp Exp
  | Var

data Op
 = Add
 | Sub

-- | 21 + 21
simpleExp :: Exp
simpleExp = App Add (Lit 21) (Lit 21)

-- | x = (x + ((21 + 21) - 2))
lambdaExp :: Exp
lambdaExp = App Add
  Var
  (App Sub
    simpleExp
    (Lit 2))

runExp :: Int -> Exp -> Int
runExp _ (Lit x) = x
runExp i (App Add lhs rhs) =
  (runExp i lhs) + (runExp i rhs)
runExp i (App Sub lhs rhs) =
  (runExp i lhs) - (runExp i rhs)
runExp i (Var) = i

run :: Int -> Int
run i = runExp i lambdaExp
