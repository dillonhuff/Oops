module RedundantControlTests() where

import Data.List as L
import Language.Java.Parser

import Issue
import RedundantControl
import TestUtils
import Utils

redundantControlTests =
  testFunction (checkStmt checkers) redundantControlCases

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
