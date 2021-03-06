module SystemTest(main) where

import Data.Either as E
import Data.List as L
import Data.Time.LocalTime
import Language.Java.Parser

import CheckCompilationUnit
import FileManipulation
import Issue

--projectDir = "/Users/dillon/JavaWorkspace"
projectDir = "/Users/dillon/javaTestProjects/clojure"

main :: IO ()
main = do
  startTime <- getZonedTime
  source <- allFilesWithExtensions javaExtensions projectDir
  res <- mapM (applyToFileContentsIO parseAndCheckCU parseLog) source
  endTime <- getZonedTime
  putStrLn $ "\n\n********************* FINAL ERROR REPORT **************************"
  putStrLn $ showIssues $ L.concat $ E.rights res
  putStrLn $ "*******************************************************************"

parseLog :: Either String [Issue] -> FilePath -> String
parseLog res path =
  case res of
    Left err -> "Parse error: " ++ err
    Right issues -> "Checked " ++ path ++ " result was:\n" ++ showIssues issues

parseAndCheckCU :: String -> IO (Either String [Issue])
parseAndCheckCU str =
  case parser compilationUnit str of
    Left err -> return $ Left $ show err
    Right compUnit -> do
      issues <- checkCompilationUnit compUnit
      return $ Right issues

