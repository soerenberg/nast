module IntegrationTests.AnnotatedEmpty (tests) where

import Text.Nast.AnnotatedAST
  ( StanProgram (..)
  , ProgramBlock (..)
  , CodeAnnotation (..)
  )

import Text.Nast.Parser (stanProgram)

import Text.ParserCombinators.Parsec (parse)

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import System.IO.Unsafe (unsafePerformIO)


annotatedBlock :: String -> String -> String -> String -> ProgramBlock
annotatedBlock a b c d = ProgramBlock [LineBased a]
                         [LineBased b, LineBased c]
                         []
                         [LineBased d, Newline]

expected :: StanProgram
expected = StanProgram
  { header = [Bracketed " header comment\n   subtitle\n" , Newline , Newline]
  , function_block = Nothing
  , data_block = Just $ annotatedBlock " after data keyword"
                                       " after data {"
                                       " in data"
                                       " after data }"
  , transformed_data_block =
      Just $ annotatedBlock " after transformed data keyword"
                            " after transformed data {"
                            " in transformed data"
                            " after transformed data }"
  , parameters_block = Just $ annotatedBlock " after parameters keyword"
                                             " after parameters {"
                                             " in parameters"
                                             " after parameters }"
  , transformed_parameters_block =
      Just $ annotatedBlock " after transformed parameters keyword"
                            " after transformed parameters {"
                            " in transformed parameters"
                            " after transformed parameters }"
  , model_block = Just $ annotatedBlock " after model keyword"
                                        " after model {"
                                        " in model"
                                        " after model }"
  , generated_quantities_block =
      Just $ annotatedBlock " after generated quantities keyword"
                            " after generated quantities {"
                            " in generated quantities"
                            " after generated quantities }"
  }

source :: String
source = unsafePerformIO $ readFile "test/IntegrationTests/AnnotatedEmpty.stan"

tests :: [TestTree]
tests =
  [ testCase "end2end parse" $
    parse stanProgram "" source
    @?= (Right $ expected)
  ]
