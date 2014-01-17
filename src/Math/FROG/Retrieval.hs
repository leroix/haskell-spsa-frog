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
    let niter = 50000
    let a = 0.2
    let c = 0.00001

    let sz = LA.rows measurement
    let gainA = SPSA.standardAk a (niter `quot` 10) 0.602
    let gainC = SPSA.standardCk c 0.101
    let lossFn = calcLoss nonlin measurement

    spsa <- SPSA.mkUnconstrainedSPSA lossFn gainA gainC (2*sz)
    initialGuess <- FR.mkInitialGuess sz

    return $ SPSA.optimize spsa niter initialGuess
    



calcLoss :: Nonlinearity -> LA.Matrix Double -> LA.Vector Double -> Double
calcLoss nonlin meas v = (/ (fromIntegral $ LA.rows meas)) . sqrt 
                                                           . LA.sumElements $ sqDiff
    where
    field = FR.flatPolar2Complex v
    (signal, gate) = FR.mkSigGate nonlin field field
    sqDiff = (^2) . abs $ (meas - FR.mkTrace signal gate)
