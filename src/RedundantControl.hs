module RedundantControl(redundantIfThenElse,
                        checkStmt,
                        checkers,
                        emptyIf) where

import Data.List as L
import Data.Maybe as M
import Language.Java.Syntax

import Issue

checkers = [redundantIfThenElse,
            emptyIf,
            emptyElse,
            checkConstantInIfCondition,
            emptyWhileLoopBody,
            checkSwitchCaseIsSameAsDefault]

checkStmt :: [Stmt -> Maybe Issue] -> Stmt -> [Issue]
checkStmt issueCheckers s =
  M.catMaybes $ L.map (\checker -> checker s) issueCheckers

redundantIfThenElse :: Stmt -> Maybe Issue
redundantIfThenElse s@(IfThenElse _ iSt eSt) = 
  case iSt == eSt of
    True -> Just $ redundantITE s
    False -> Nothing
redundantIfThenElse _ = Nothing

emptyIf :: Stmt -> Maybe Issue
emptyIf s@(IfThenElse _ iSt _) =
  case iSt of
    Empty -> Just $ redundantIf s
    (StmtBlock (Block [])) -> Just $ redundantIf s
    _ -> Nothing
emptyIf _ = Nothing

emptyElse :: Stmt -> Maybe Issue
emptyElse s@(IfThenElse _ _ eSt) =
  case eSt of
    Empty -> Just $ redundantElse s
    (StmtBlock (Block [])) -> Just $ redundantElse s
    _ -> Nothing
emptyElse _ = Nothing

checkConstantInIfCondition :: Stmt -> Maybe Issue
checkConstantInIfCondition s@(IfThenElse c _ _) =
  case c of
    (Lit (Boolean _)) -> Just $ constantInIfCondition s
    _ -> Nothing
checkConstantInIfCondition s@(IfThen c _) =
  case c of
    (Lit (Boolean _)) -> Just $ constantInIfCondition s
    _ -> Nothing
checkConstantInIfCondition _ = Nothing

emptyWhileLoopBody s@(While _ lSt) =
  case lSt of
    Empty -> Just $ emptyLoopBody s
    (StmtBlock (Block [])) -> Just $ emptyLoopBody s
    _ -> Nothing
emptyWhileLoopBody _ = Nothing

checkSwitchCaseIsSameAsDefault s@(Switch _ swBlks) =
  case switchBlockWithDuplicateOfDefault swBlks of
    True -> Just $ switchCaseIsSameAsDefault s
    False -> Nothing
checkSwitchCaseIsSameAsDefault _ = Nothing

switchBlockWithDuplicateOfDefault swBlks =
  case getDefault swBlks of
    Nothing -> False
    Just d -> L.or $ L.map (compareToDefault d) swBlks

getDefault [] = Nothing
getDefault ((SwitchBlock Default b) : rest) = Just $ SwitchBlock Default b
getDefault (x:rest) = getDefault rest

compareToDefault (SwitchBlock Default _) (SwitchBlock Default _) = False
compareToDefault (SwitchBlock Default b1) (SwitchBlock _ b2) = b1 == b2
      
