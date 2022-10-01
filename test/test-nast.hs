module Main
  ( main
  , tests
  ) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Tests.Parsers.Annotations
import qualified Tests.Parsers.Literals


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "nast tests"
  [ testGroup "Parser.Annotations" Tests.Parsers.Annotations.tests
  , testGroup "Parser.Literals" Tests.Parsers.Literals.tests
  ]
