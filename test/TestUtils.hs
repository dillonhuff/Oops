module TestUtils(testFunction, testFunctionM) where

import Data.List as L
import Test.HUnit

testFunction func cases = runTestTT $ makeTestCases func cases

makeTestCases func cases =
  TestList $ map (\(input, expected) -> testCase func input expected) cases

testCase func input expected =
  TestCase (assertEqual ("Input: " ++ show input) expected (func input))

testFunctionM :: (Show a, Show b, Eq b, Monad m) => (a -> m b) -> [(a, b)] -> m String
testFunctionM f cases = do
  computedValues <- mapM f $ L.map fst cases
  let expectedValues = L.map snd cases in
    showResults computedValues expectedValues

showResults [] [] = return "Test Passed"
showResults (c:cs) (e:es) =
  case c == e of
    True -> showResults cs es
    False -> return $ "Failed, expected " ++ show e ++ " but got " ++ show c

  
