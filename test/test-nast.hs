module Main
  ( main
  , tests
  ) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Tests.Parser


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "nast tests"
  [ testGroup "Parser" Tests.Parser.tests
  ]
