module Issue(Issue,
             redundantITE,
             redundantIf,
             redundantElse,
             constantInIfCondition,
             emptyLoopBody,
             switchCaseIsSameAsDefault,
             showIssues) where

import Data.List as L
import Language.Java.Pretty
import Language.Java.Syntax

data Issue
  = RedundantITE Stmt
  | RedundantIf Stmt
  | RedundantElse Stmt
  | ConstantInIfCondition Stmt
  | EmptyLoopBody Stmt
  | SwitchCaseIsSameAsDefault Stmt
    deriving (Eq, Ord)

instance Show Issue where
  show (RedundantITE s) = "Redundant if-else statment:\n" ++ showP s
  show (RedundantIf s) = "Redundant if:\n" ++ showP s
  show (RedundantElse s) = "Redundant else:\n" ++ showP s
  show (ConstantInIfCondition s) = "Constant in if condition:\n" ++ showP s
  show (EmptyLoopBody s) = "Empty loop body:\n" ++ showP s
  show (SwitchCaseIsSameAsDefault s) = "Switch case is same as default:\n" ++ showP s

showP s = show $ pretty s

redundantITE s = RedundantITE s
redundantIf s = RedundantIf s
redundantElse s = RedundantElse s
constantInIfCondition s = ConstantInIfCondition s
emptyLoopBody s = EmptyLoopBody s
switchCaseIsSameAsDefault s = SwitchCaseIsSameAsDefault s

showIssues :: [Issue] -> String
showIssues issues = L.concat $ L.intersperse "\n\n" $ L.map show issues
