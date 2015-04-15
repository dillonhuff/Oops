module RedundantControl(redundantIfThenElse) where

import Language.Java.Syntax

import Issue

redundantIfThenElse :: Stmt -> Maybe Issue
redundantIfThenElse x =
  case x of
    (IfThenElse _ iSt eSt) -> case iSt == eSt of
      True -> Just $ issue x
      False -> Nothing
    _ -> Nothing
