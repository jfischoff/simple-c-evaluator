module Main where

import Language.C.Simple.Evaluator
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Text.Parsec
import Debug.Trace.Helpers
import Debug.Trace    
import Text.PrettyPrint.Mainland

main = defaultMain tests

tests = [
        testGroup "EvaluatorTest" [
        ]
    ]
