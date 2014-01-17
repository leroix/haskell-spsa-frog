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

type PulseFn = Double -> Double -> LA.Complex Double

-----------------
-- Example Pulses
-----------------
singleGaus :: PulseFn
singleGaus sz k = exp ((-pi) / sz * (k - sz/2.0)^2) LA.:+ 0.0

doubleGaus :: PulseFn
doubleGaus sz k = (singleGaus sz (k - (sz / 8.0))) + (singleGaus sz (k + (sz / 8.0)))

mkPulse :: PulseFn -> Int -> ComplexSignal
mkPulse form sz = LA.buildVector sz mkElem
    where
    dsz = (fromIntegral sz) :: Double
    mkElem = form dsz . fromIntegral



caseRetrieval :: ComplexSignal -> IO ()
caseRetrieval field = do
    let sz = LA.dim field
    let dsz = (fromIntegral sz) :: Double
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
    [ testGroup "Retrieval" [ testCase "Gaussian" $ caseRetrieval (mkPulse singleGaus 32)
                            , testCase "Double Gaussian" $ caseRetrieval (mkPulse doubleGaus 32)
                            ]
    ]


-- convenient whilst in ghci
runAllTests :: IO ()
runAllTests = defaultMainWithArgs tests []

runTests :: String -> IO ()
runTests p = defaultMainWithArgs tests ["--select-tests", p]

runGroup :: Int -> IO ()
runGroup i = defaultMainWithArgs [tests !! i] []
