module RedundantControlTests() where

import Data.List as L
import Language.Java.Parser

import Issue
import RedundantControl
import TestUtils
import Utils

redundantITETests =
  testFunction redundantIfThenElse supITECases

supITECases = L.zip stmts ress

stmts = L.map extractRight $ L.map (\(x, y) -> parser stmt x) rawCases

ress = L.map snd rawCases

rawCases =
  [("return 12;", Nothing),
   ("if (x) return 1; else return 1;", Just $ issue $ stmts !! 1)]
