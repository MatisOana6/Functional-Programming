{-# LANGUAGE GADTs #-}

module Test.SimpleTest where

import Data.Fixed (Centi, Fixed (MkFixed))
import qualified Data.List as L
import Test.SimpleTest.Expectation
import Test.SimpleTest.TestCase
import Text.Printf (printf)

group :: (Show a) => String -> [TestTree a] -> TestTree a
group name items = TestTreeNode {testGroupName = name, testGroupChildren = items}

testCase :: Expectation e => String -> Int -> e -> TestTree TestCase
testCase name points expectation =
  TestTreeLeaf TestCase {testCaseName = name, testCaseExpectation = expectation, testCasePoints = MkFixed (fromIntegral points)}

runTest :: TestCase -> Bool -> TestResult
runTest tc@(TestCase _ expectation _) showDetails
  | holds expectation = Passed tc
  | otherwise = Failed tc showDetails

runTestGroup :: Bool -> TestTree TestCase -> TestTree TestResult
runTestGroup showDetails (TestTreeLeaf tc) = TestTreeLeaf (runTest tc showDetails)
runTestGroup showDetails (TestTreeNode name tts) = TestTreeNode name (fmap (runTestGroup showDetails) tts)

calculateScore :: TestTree TestResult -> TestPoints Centi
calculateScore (TestTreeLeaf (Passed tc)) = TestPoints pts pts where pts = testCasePoints tc
calculateScore (TestTreeLeaf (Failed tc _)) = TestPoints 0 pts where pts = testCasePoints tc
calculateScore (TestTreeNode s tts) = foldMap calculateScore tts

evalTestGroup :: Bool -> TestTree TestCase -> IO ()
evalTestGroup showDetails gr = do
  let results = runTestGroup showDetails gr
      score = calculateScore results
  print results
  printf "Final score: %s\n" (show score)
