module BooleanFormula(true, false, con, dis, neg, var,
                      expToBooleanFormula,
                      isTautology,
                      expIsTautology) where

import Language.Java.Syntax
import Z3.Monad

data BooleanFormula
  = T
  | F
  | Var String
  | Con BooleanFormula BooleanFormula
  | Dis BooleanFormula BooleanFormula
  | Neg BooleanFormula BooleanFormula
    deriving (Eq, Ord, Show)

true = T
false = F
var = Var
con = Con
dis = Dis
neg = Neg

expToBooleanFormula :: Exp -> Maybe BooleanFormula
expToBooleanFormula (Lit (Boolean True)) = Just true
expToBooleanFormula (Lit (Boolean False)) = Just false

expIsTautology :: Exp -> IO Bool
expIsTautology exp =
  case expToBooleanFormula exp of
    Just bf -> isTautology bf
    Nothing -> return False

isTautology :: BooleanFormula -> IO Bool
isTautology bf = evalZ3 $ isTautologyZ3 bf

isTautologyZ3 :: BooleanFormula -> Z3 Bool
isTautologyZ3 bf = do
  z3bf <- toZ3Formula bf
  negz3bf <- mkNot z3bf
  assert negz3bf
  res <- check
  case res of
    Unsat -> return True
    _ -> return False

toZ3Formula T = mkTrue
toZ3Formula F = mkFalse
