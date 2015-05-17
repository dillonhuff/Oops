module BooleanFormulaTests(allBooleanFormulaTests) where

import Data.List as L
import Language.Java.Syntax

import BooleanFormula
import TestUtils

allBooleanFormulaTests = do
  testFunction expToBooleanFormula expToBoolSuccessCases
  testFunctionM isTautology isTautCases

expToBoolSuccessCases =
  L.map (\(x, y) -> (x, Just y))
  [(Lit $ Boolean True, true),
   (Lit $ Boolean False, false)]

isTautCases =
  [(true, True),
   (false, False)]
