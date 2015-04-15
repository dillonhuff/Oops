module CheckCompilationUnitTests() where

import Language.Java.Parser

import CheckCompilationUnit
import Issue
import TestUtils
import Utils

checkCompilationUnitTests =
  testFunction (length . checkCompilationUnit) compUnitCases

compUnitCases =
  [(emptyClass, 0),
   (redundantITEInMethod, 1)]

emptyClass = extractRight $ parser compilationUnit "public class X {}"

redundantITEInMethod =
  extractRight $ parser compilationUnit
  "public class X { public int x() { if (k) { return k + 1; } else { return k + 1; }}}"
