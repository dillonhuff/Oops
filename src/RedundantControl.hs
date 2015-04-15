module RedundantControl(redundantIfThenElse,
                        checkStmt,
                        checkers,
                        emptyIf) where

import Data.List as L
import Data.Maybe as M
import Language.Java.Syntax

import Issue

checkers = [redundantIfThenElse, emptyIf, emptyElse]

checkStmt :: [Stmt -> Maybe Issue] -> Stmt -> [Issue]
checkStmt issueCheckers s =
  M.catMaybes $ L.map (\checker -> checker s) issueCheckers

redundantIfThenElse :: Stmt -> Maybe Issue
redundantIfThenElse s@(IfThenElse _ iSt eSt) = 
  case iSt == eSt of
    True -> Just $ issue s
    False -> Nothing
redundantIfThenElse _ = Nothing

emptyIf :: Stmt -> Maybe Issue
emptyIf s@(IfThenElse _ iSt _) =
  case iSt of
    Empty -> Just $ issue s
    (StmtBlock (Block [])) -> Just $ issue s
    _ -> Nothing
emptyIf _ = Nothing

emptyElse :: Stmt -> Maybe Issue
emptyElse s@(IfThenElse _ _ eSt) =
  case eSt of
    Empty -> Just $ issue s
    (StmtBlock (Block [])) -> Just $ issue s
    _ -> Nothing
emptyElse _ = Nothing
