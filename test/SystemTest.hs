module SystemTest(main) where

import Data.List as L
import Data.Time.LocalTime
import Language.Java.Parser

import CheckCompilationUnit
import FileManipulation

projectDir = "/Users/dillon/javaTestProjects/openJDKSandbox/9dev"

main :: IO ()
main = do
  startTime <- getZonedTime
  source <- allFilesWithExtensions javaExtensions projectDir
  res <- mapM (applyToFileContents parseAndCheckCU parseLog) source
  endTime <- getZonedTime
  return ()

parseLog :: String -> FilePath -> String
parseLog res path =
  "Checked " ++ path ++ " result was:\n" ++ res

parseAndCheckCU str =
  case parser compilationUnit str of
    Left err -> "Parse error: " ++ show err ++ "\n"
    Right compUnit -> (show $ checkCompilationUnit compUnit) ++ "\n"
