module Test.Test where

import Test.Framework (defaultMainWithArgs, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Property, NonNegative(..))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.HUnit (assertBool)

import qualified Numeric.LinearAlgebra as LA
import qualified Graphics.Rendering.Plot.HMatrix as G

import qualified Math.FROG.Tools as FR
import Math.FROG.Retrieval
import Math.FROG.Types
--import Test.Types


caseGaussianRetrieval :: Int -> IO ()
caseGaussianRetrieval sz = do
    let dsz = (fromIntegral sz) :: Double
    let field = LA.buildVector sz (\k -> exp((-pi) / dsz * ((fromIntegral k) - dsz/2.0)^2) LA.:+ 0.0)
    let trc = FR.mkTrace field field

    o <- retrieve SHG trc

    let rfield = FR.center . FR.flatPolar2Complex $ o
    let mags = (LA.mapVector LA.magnitude) rfield

    putStrLn . show $ calcLoss SHG trc o

    G.mplot [LA.linspace sz (0::Double, dsz-1), mags]

    G.imshow trc

    G.imshow (FR.mkTrace rfield rfield)


-----------------
---- List of Tests
-----------------

tests :: [Test]
tests = 
    [ testGroup "Retrieval" [ testCase "Gaussian Retrieval" $ caseGaussianRetrieval 32
                            ]
    ]


-- convenient whilst in ghci
runAllTests :: IO ()
runAllTests = defaultMainWithArgs tests []

runTests :: String -> IO ()
runTests p = defaultMainWithArgs tests ["--select-tests", p]

runGroup :: Int -> IO ()
runGroup i = defaultMainWithArgs [tests !! i] []
