module RedundantControlTests() where

import Data.List as L
import Language.Java.Parser

import Issue
import RedundantControl
import TestUtils
import Utils

redundantControlTests = do
  testFunction (checkStmtForRedundantControl pureCheckers) redundantControlCases
  testFunctionM (checkStmtForRedundantControlM impureCheckers) impureRedundantControlCases

redundantControlCases = L.zip stmts ress

stmts = L.map extractRight $ L.map (\(x, y) -> parser stmt x) rawCases

ress = L.map snd rawCases

rawCases =
  [("return 12;", []),
   ("if (x) return 1; else return 1;", [redundantITE $ stmts !! 1]),
   ("if (x) {} else { return 1; }", [redundantIf $ stmts !! 2]),
   ("if (x) { return k; } else {}", [redundantElse $ stmts !! 3]),
   ("if (true) { x = 1; } else { x = 2; }", [constantInIfCondition $ stmts !! 4]),
   ("if (true) { x = 1; }", [constantInIfCondition $ stmts !! 5]),
   ("while (x > 4) {}", [emptyLoopBody $ stmts !! 6]),
   ("switch (x) { case(1): y = 3; break; default: y = 3; break; }", [switchCaseIsSameAsDefault $ stmts !! 7])]

impureRedundantControlCases = L.zip impureStmts impureRess

impureStmts = L.map extractRight $ L.map (\(x, y) -> parser stmt x) rawImpureRedundantControlCases

impureRess = L.map snd rawImpureRedundantControlCases

rawImpureRedundantControlCases =
  [("return x;", []),
   ("if (true) { return x; }", [conditionExpressionIsTautology $ impureStmts !! 1]),
   ("if (true) {return x;} else {return y;}", [conditionExpressionIsTautology $ impureStmts !! 2]),
   ("while (true) { x = x + 1; }", []),
   ("assert true : 2;", [conditionExpressionIsTautology $ impureStmts !! 4]),
   ("do { x = x + 4; } while (true);", [conditionExpressionIsTautology $ impureStmts !! 5])]
