module Test.Test where

import Test.Framework (defaultMainWithArgs, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Property, NonNegative(..))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.HUnit (assertBool)

import qualified Numeric.LinearAlgebra as LA

import qualified Math.FROG.Tools as FR
import Math.FROG.Retrieval (retrieve)
--import Test.Types


gen

-- convenient whilst in ghci
runAllTests :: IO ()
runAllTests = defaultMainWithArgs tests []

runTests :: String -> IO ()
runTests p = defaultMainWithArgs tests ["--select-tests", p]

runGroup :: Int -> IO ()
runGroup i = defaultMainWithArgs [tests !! i] []
