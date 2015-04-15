module Issue(Issue,
             issue) where

import Language.Java.Syntax

data Issue = Issue Stmt
             deriving (Eq, Ord, Show)

issue = Issue
