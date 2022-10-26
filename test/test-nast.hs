module Main
  ( main
  , tests
  ) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Tests.Parser
import qualified IntegrationTests.AnnotatedEmpty
import qualified IntegrationTests.Empty
import qualified IntegrationTests.Schools


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "nast tests"
  [ testGroup "Parser" Tests.Parser.tests
  , testGroup "Integration Empty" IntegrationTests.Empty.tests
  , testGroup "Integration AnnotatedEmpty" IntegrationTests.AnnotatedEmpty.tests
  , testGroup "Integration 8 Schools" IntegrationTests.Schools.tests
  ]
