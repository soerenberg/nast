{-# LANGUAGE OverloadedStrings #-}
module IntegrationTests.Schools (tests) where

import Text.Nast.AnnotatedAST
  ( StanProgram (..)
  , ProgramBlock (..)
  , CodeAnnotation (..)
  , Expr (..)
  , Stmt (..)
  , VarConstraint (..)
  , VarConstraints (..)
  , VarType (..)
  , ArrayDims (..)
  )

import Text.Nast.Parser (stanProgram)

import Text.Parsec (parse)

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Text (Text, pack)
import System.IO.Unsafe (unsafePerformIO)


tests :: [TestTree]
tests =
  [ testCase "end2end parse" $
    parse stanProgram "" source
    @?= (Right $ expected)
  ]

source :: Text
source = pack $ unsafePerformIO $ readFile "test/IntegrationTests/Schools.stan"

expected :: StanProgram
expected = StanProgram
  { header                       = [LineBased " Eight schools model"]
  , function_block               = Nothing
  , data_block                   = Just dataBlock
  , transformed_data_block       = Nothing
  , parameters_block             = Just parametersBlock
  , transformed_parameters_block = Just transformedParametersBlock
  , model_block                  = Just modelBlock
  , generated_quantities_block   = Nothing
  }

dataBlock :: ProgramBlock
dataBlock = ProgramBlock [] [Newline]
             [ VarDecl (Int [] lower0)
                       (Identifier "N" [])
                       Nothing
                       [LineBased " num groups"]
             , VarDecl (Real [] Nothing)
                       (Identifier "y" [])
                       (Just $ ArrayDims [([], Identifier "N" [])] [])
                       [LineBased " observations"]
             , VarDecl (Real [] lower0)
                       (Identifier "sigma" [])
                       (Just $ ArrayDims [([], Identifier "N" [])] [])
                       [LineBased " group std dev"]
             ] [Newline]

parametersBlock :: ProgramBlock
parametersBlock = ProgramBlock [] [Newline]
                    [ VarDecl (Real [] Nothing)
                              (Identifier "alpha" [])
                              Nothing
                              [LineBased " population mean"]
                    , VarDecl (Real [] lower0)
                              (Identifier "gamma" [])
                              Nothing
                              [LineBased " population std dev"]
                    , VarDecl (Vector [] Nothing [] (Identifier "N" []) [])
                              (Identifier "mu" [])
                              Nothing
                              [LineBased " uncentered group means"]
                    ] [Newline]

transformedParametersBlock :: ProgramBlock
transformedParametersBlock =
  ProgramBlock [] [Newline]
    [ VarDeclAssign
        (Vector [] Nothing [] (Identifier "N" []) [])
        (Identifier "mu_cent" [])
        Nothing
        []
        (Add (Identifier "alpha" [])
             []
             (Mul (Identifier "gamma" [])
                  []
                  (Identifier "mu" [])
             ))
        [LineBased " centered group means"]
    ] [Newline]

modelBlock :: ProgramBlock
modelBlock =
  ProgramBlock [] [Newline, LineBased " prior model"]
    [ Tilde (Identifier "mu" [])
            []
            (Call (Identifier "normal" []) [lit_0, lit_1] [[], []] [])
            [Newline, Newline, LineBased " likelihood model"]
    , Tilde (Identifier "y" [])
            []
            (Call (Identifier "normal" []) [ Identifier "mu_cent" []
                                           , Identifier "sigma" []] [[], []] [])
            [Newline]
    ] [Newline]

lit_0 :: Expr
lit_0 = NumLiteral "0" Nothing Nothing []

lit_1 :: Expr
lit_1 = NumLiteral "1" Nothing Nothing []

lower0 :: Maybe VarConstraints
lower0 = Just $ VarConstraints [Lower [] [] [] lit_0] []
