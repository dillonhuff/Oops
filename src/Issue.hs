module Issue(Issue,
             redundantITE,
             redundantIf,
             redundantElse,
             showIssues) where

import Data.List as L
import Language.Java.Pretty
import Language.Java.Syntax

data Issue
  = RedundantITE Stmt
  | RedundantIf Stmt
  | RedundantElse Stmt
    deriving (Eq, Ord)

instance Show Issue where
  show (RedundantITE s) = "Redundant if-else statment:\n" ++ (show $ pretty s)
  show (RedundantIf s) = "Redundant if:\n" ++ (show $ pretty s)
  show (RedundantElse s) = "Redundant else:\n" ++ (show $ pretty s)

redundantITE s = RedundantITE s
redundantIf s = RedundantIf s
redundantElse s = RedundantElse s

showIssues :: [Issue] -> String
showIssues issues = L.concat $ L.intersperse "\n\n" $ L.map show issues
