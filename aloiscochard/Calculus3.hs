module Calculus3 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Exp
  = Lit Int
  | App Exp Exp
  | Var Name
  | Lam String Exp

data Name
  = MName Op        -- | Machine name     (primitive operations)
  | IName String    -- | Internal name    (local variable)

data Op
 = Add
 | Sub

type Scope = Map String Exp

-- | 21 + 21
simpleExp :: Exp
simpleExp =
  App
    (Var (MName Add))
    (App
      (Lit 21)
      (Lit 21))

-- | x = (x + ((21 + 21) - 2))
lambdaExp :: Exp
lambdaExp =
  Lam "x"
    (App
      (Var (MName Add))
      (App
        (Var (IName "x"))
        (App
          (Var (MName Sub))
          (App
            simpleExp
            (Lit 2)))))

runExp :: Scope -> Exp -> Int
runExp _ (Lit i) = i
runExp s (Var (IName n)) =
  case Map.lookup n s of
    Nothing   -> error ("Not in scope: " ++ n)
    Just exp  -> runExp s exp
runExp s (App (Var (MName op)) (App lhs rhs)) =
  case op of
    Add -> lhs' + rhs'
    Sub -> lhs' - rhs'
  where
    lhs' = runExp s lhs
    rhs' = runExp s rhs
runExp s (App (Lam n exp) arg) = runExp s' exp
  where
    s' = Map.insert n arg s

run :: Int -> Int
run i = runExp Map.empty (App lambdaExp (Lit i))
