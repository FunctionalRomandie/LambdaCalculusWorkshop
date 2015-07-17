module Calculus2 where

data Exp
  = Lit Int
  | App Exp Exp
  | Var Op

data Op
 = Add
 | Sub

-- | 21 + 21
simpleExp :: Exp
simpleExp =
  App
    (Var Add)
    (App
      (Lit 21)
      (Lit 21))

runExp :: Exp -> Int
runExp (Lit i) = i
runExp (App (Var op) (App lhs rhs)) =
  case op of
    Add -> lhs' + rhs'
    Sub -> lhs' - rhs'
  where
    lhs' = runExp lhs
    rhs' = runExp rhs

run :: Int
run = runExp simpleExp
