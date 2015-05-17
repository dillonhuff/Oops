module CheckCompilationUnit(checkCompilationUnit) where

import Control.Monad
import Data.List as L
import Language.Java.Syntax

import Issue
import RedundantControl

checkCompilationUnit :: CompilationUnit -> IO [Issue]
checkCompilationUnit (CompilationUnit mPkg imports typeDecls) =
  concatMapM checkTypeDecl typeDecls

checkTypeDecl :: TypeDecl -> IO [Issue]
checkTypeDecl (ClassTypeDecl c) =
  checkClassDecl c
checkTypeDecl (InterfaceTypeDecl i) =
  checkInterfaceDecl i

checkClassDecl (ClassDecl _ _ _ _ _ body) =
  checkClassBody body
checkClassDecl _ = return []

checkInterfaceDecl _ = return []

checkClassBody (ClassBody decls) =
  concatMapM checkDecl decls

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
checkMemberDecl (FieldDecl _ _ _) = return []

checkMethodBody (MethodBody (Just blk)) = checkBlock blk
checkMethodBody _ = return []

checkConstructorBody (ConstructorBody _ blkStmts) =
  concatMapM checkBlkStmt blkStmts

checkBlock (Block stmts) = concatMapM checkBlkStmt stmts

checkBlkStmt :: BlockStmt -> IO [Issue]
checkBlkStmt (BlockStmt st) = checkStmt st
checkBlkStmt (LocalClass decl) = checkClassDecl decl
checkBlkStmt (LocalVars _ _ _) = return []

checkStmtR :: Stmt -> IO [Issue]
checkStmtR (StmtBlock b) = checkBlock b
checkStmtR (IfThen _ s) = checkStmt s
--checkStmtR (IfThenElse _ l r) = checkStmt l ++ checkStmt r
checkStmtR (While _ s) = checkStmt s
checkStmtR (BasicFor _ _ _ s) = checkStmt s
checkStmtR (EnhancedFor _ _ _ _ s) = checkStmt s
--checkStmtR (Switch _ switchBlks) = L.concatMap checkSwitchBlock switchBlks
checkStmtR (Synchronized _ blk) = checkBlock blk
--checkStmtR (Try blk catches (Just cBlk)) = checkBlock blk ++ L.concatMap checkCatch catches ++ checkBlock cBlk
--checkStmtR (Try blk catches Nothing) = checkBlock blk ++ L.concatMap checkCatch catches
checkStmtR (Labeled _ s) = checkStmt s
checkStmtR _ = return []

checkSwitchBlock (SwitchBlock _ blkStmts) = error "checkSwitchBlock not implemented"
--  L.concatMap checkBlkStmt blkStmts

checkCatch (Catch _ blk) = checkBlock blk

checkStmt :: Stmt -> IO [Issue]
checkStmt s = checkStmtForRedundantControlAll s

-- This should really exist in Control.Monad
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = liftM L.concat $ sequence $ L.map f as
