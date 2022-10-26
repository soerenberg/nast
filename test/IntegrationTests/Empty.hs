{-# LANGUAGE OverloadedStrings #-}
module IntegrationTests.Empty (tests) where

import Text.Nast.AnnotatedAST
  ( StanProgram (..)
  , ProgramBlock (..)
  , CodeAnnotation (..)
  )

import Text.Nast.Parser (stanProgram)

import Text.Parsec (parse)

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Text (Text, pack)
import System.IO.Unsafe (unsafePerformIO)


emptyBlock :: ProgramBlock
emptyBlock = ProgramBlock [] [Newline] [] [Newline]

expected :: StanProgram
expected = StanProgram
  { header                       = []
  , function_block               = Nothing
  , data_block                   = Just emptyBlock
  , transformed_data_block       = Just emptyBlock
  , parameters_block             = Just emptyBlock
  , transformed_parameters_block = Just emptyBlock
  , model_block                  = Just emptyBlock
  , generated_quantities_block   = Just emptyBlock
  }

source :: Text
source = pack $ unsafePerformIO $ readFile "test/IntegrationTests/Empty.stan"

tests :: [TestTree]
tests =
  [ testCase "end2end parse" $
    parse stanProgram "" source
    @?= (Right $ expected)
  ]
