module CheckCompilationUnit(checkCompilationUnit) where

import Data.List as L
import Language.Java.Syntax

import Issue
import RedundantControl

checkCompilationUnit :: CompilationUnit -> [Issue]
checkCompilationUnit (CompilationUnit mPkg imports typeDecls) =
  L.concatMap checkTypeDecl typeDecls

checkTypeDecl :: TypeDecl -> [Issue]
checkTypeDecl (ClassTypeDecl c) =
  checkClassDecl c
checkTypeDecl (InterfaceTypeDecl i) =
  checkInterfaceDecl i

checkClassDecl (ClassDecl _ _ _ _ _ body) =
  checkClassBody body
checkClassDecl _ = []

checkInterfaceDecl _ = []

checkClassBody (ClassBody decls) =
  L.concatMap checkDecl decls

checkDecl (MemberDecl m) = checkMemberDecl m
checkDecl (InitDecl _ blk) = checkBlock blk

checkMemberDecl (MethodDecl _ _ _ _ _ _ body) =
  checkMethodBody body
checkMemberDecl (ConstructorDecl _ _ _ _ _ body) =
  checkConstructorBody body
checkMemberDecl (MemberClassDecl decl) =
  checkClassDecl decl
checkMemberDecl (MemberInterfaceDecl decl) =
  checkInterfaceDecl decl
checkMemberDecl (FieldDecl _ _ _) = []

checkMethodBody (MethodBody (Just blk)) = checkBlock blk
checkMethodBody _ = []

checkConstructorBody (ConstructorBody _ blkStmts) =
  L.concatMap checkBlkStmt blkStmts

checkBlock (Block stmts) =
  L.concatMap checkBlkStmt stmts

checkBlkStmt (BlockStmt st) = checkStmt checkers st
checkBlkStmt (LocalClass decl) = checkClassDecl decl
checkBlkStmt (LocalVars _ _ _) = []

checkStmtR (StmtBlock b) = checkBlock b
checkStmtR (IfThen _ s) = checkStmt checkers s
checkStmtR (IfThenElse _ l r) = checkStmt checkers l ++ checkStmt checkers r
checkStmtR (While _ s) = checkStmt checkers s
checkStmtR (BasicFor _ _ _ s) = checkStmt checkers s
checkStmtR (EnhancedFor _ _ _ _ s) = checkStmt checkers s
checkStmtR (Switch _ switchBlks) = L.concatMap checkSwitchBlock switchBlks
checkStmtR (Synchronized _ blk) = checkBlock blk
checkStmtR (Try blk catches (Just cBlk)) = checkBlock blk ++ L.concatMap checkCatch catches ++ checkBlock cBlk
checkStmtR (Try blk catches Nothing) = checkBlock blk ++ L.concatMap checkCatch catches
checkStmtR (Labeled _ s) = checkStmt checkers s
checkStmtR _ = []

checkSwitchBlock (SwitchBlock _ blkStmts) =
  L.concatMap checkBlkStmt blkStmts

checkCatch (Catch _ blk) = checkBlock blk
