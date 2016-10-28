{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Kcd.Parser

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Parsing"
  [ testCase "Test file" $ do
      file <- parseKcdFile "test_example.kcd"
      _documentName (_networkDefinitionDocument file) @?= Just "???"
  ]
