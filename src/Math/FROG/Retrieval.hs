module Math.FROG.Retrieval
( retrieve
, calcLoss
) where

import qualified Numeric.LinearAlgebra as LA
import qualified Math.Optimization.SPSA as SPSA

import qualified Math.FROG.Tools as FR
import Math.FROG.Types (Nonlinearity)

retrieve :: Nonlinearity -> LA.Matrix Double -> IO (LA.Vector Double)
retrieve nonlin measurement = do
    let sz = LA.rows measurement
    let (gainA, gainC) = SPSA.semiautomaticTuning 0.0001 0.05
    let lossFn = calcLoss nonlin measurement

    spsa <- SPSA.mkUnconstrainedSPSA lossFn gainA gainC sz
    initialGuess <- FR.mkInitialGuess sz

    return $ SPSA.optimize spsa 1000 initialGuess
    



calcLoss :: Nonlinearity -> LA.Matrix Double -> LA.Vector Double -> Double
calcLoss nonlin meas v = (/ (fromIntegral $ LA.rows meas)) . sqrt 
                                                           . LA.sumElements $ sqDiff
    where
    sz = (LA.dim v) `quot` 2
    mags = LA.subVector 0 sz v
    angs = LA.subVector sz sz v
    field = LA.buildVector sz (\k -> (LA.mkPolar (mags LA.@> k) (angs LA.@> k)))
    (signal, gate) = FR.mkSigGate nonlin field field
    sqDiff = (^2) . abs $ (meas - FR.mkTrace signal gate)
