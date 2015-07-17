module Calculus4 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Exp
  = Lit Int
  | App Exp Exp
  | Var Name
  | Lam String Exp
  | Let Bind Exp

data Name
  = MName Op        -- | Machine name     (primitive operations)
  | IName String    -- | Internal name    (local variable)

data Op
 = Add
 | Sub

data Bind = Bind String Exp

type Scope = Map String Exp

-- | let y = 21 in (x + (y + y))
letExp :: Exp
letExp =
  Lam "x"
    (Let
      (Bind "y" (Lit 21))
      (App
        (Var (MName Add))
        (App
          (Var (IName "x"))
          (App
            (Var (MName Add))
            (App
              (Var (IName "y"))
              (Var (IName "y")))))))

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
runExp s (Let (Bind n exp) prg) = runExp s' prg
  where
    s' = Map.insert n exp s

run :: Int -> Int
run i = runExp Map.empty (App letExp (Lit i))

