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
   ("if (x) { return k; } else {}", [redundantElse $ stmts !! 3])]
