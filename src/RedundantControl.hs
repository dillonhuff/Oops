module RedundantControl(redundantIfThenElse,
                        checkStmtForRedundantControl, checkStmtForRedundantControlM, checkStmtForRedundantControlAll,
                        pureCheckers, impureCheckers,
                        emptyIf) where

import Control.Monad
import Data.List as L
import Data.Maybe as M
import Language.Java.Syntax

import BooleanFormula
import Issue

pureCheckers = [redundantIfThenElse,
                emptyIf,
                emptyElse,
                checkConstantInIfCondition,
                emptyWhileLoopBody,
                checkSwitchCaseIsSameAsDefault]

impureCheckers = [conditionExpIsTautology]

checkStmtForRedundantControlAll :: Stmt -> IO [Issue]
checkStmtForRedundantControlAll s =
  let pureIssues = checkStmtForRedundantControl pureCheckers s in
  do
    impureIssues <- checkStmtForRedundantControlM impureCheckers s
    return $ pureIssues ++ impureIssues

checkStmtForRedundantControlM :: (Monad m) => [Stmt -> m (Maybe Issue)] -> Stmt -> m [Issue]
checkStmtForRedundantControlM issueCheckersM s = do
  liftM M.catMaybes $ mapM (\checker -> checker s) issueCheckersM

checkStmtForRedundantControl :: [Stmt -> Maybe Issue] -> Stmt -> [Issue]
checkStmtForRedundantControl issueCheckers s =
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
      
conditionExpIsTautology :: Stmt -> IO (Maybe Issue)
conditionExpIsTautology s@(IfThen e _) = checkExpIsTautology s e
conditionExpIsTautology s@(IfThenElse e _ _) = checkExpIsTautology s e
conditionExpIsTautology s@(Assert e _) = checkExpIsTautology s e
conditionExpIsTautology s@(Do _ e) = checkExpIsTautology s e
conditionExpIsTautology s = return Nothing

checkExpIsTautology :: Stmt -> Exp -> IO (Maybe Issue)
checkExpIsTautology s e = do
  isTaut <- expIsTautology e
  case isTaut of
    True -> return $ Just $ conditionExpressionIsTautology s
    False -> return Nothing
