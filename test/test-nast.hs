module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Tests.Parsers.Comments
import qualified Tests.Parsers.Literals


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "nast tests"
  [ testGroup "Parser.Comments" Tests.Parsers.Comments.tests
  , testGroup "Parser.Literals" Tests.Parsers.Literals.tests
  ]
