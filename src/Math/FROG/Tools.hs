module Math.FROG.Tools
( mkTrace
, mkSigGate
, shift
, mkInitialGuess
, flatPolar2Complex
, center
) where

import qualified System.Random as R
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.GSL.Fourier as F
import qualified Foreign

import Math.FROG.Types



mkTrace :: ComplexSignal -> ComplexSignal -> Trace
mkTrace field gate = LA.fromColumns $ map atEachDelayApply [upper,upper-1..lower]
    where
    atEachDelayApply = (^2) . (LA.mapVector LA.magnitude) 
                            . (flip shift upper) 
                            .  F.fft 
                            . (*field) 
                            . (shift gate)
    sz = LA.dim field
    upper = (sz `quot` 2)
    lower = (-1) * (sz `quot` 2) + 1


mkSigGate :: Nonlinearity -> ComplexSignal -> ComplexSignal -> (ComplexSignal, ComplexSignal)
mkSigGate PG f g = (id f, LA.mapVector ((^2) . abs) g)
mkSigGate SHG f g = (id f, id g) 
mkSigGate SD f g = (LA.mapVector (^2) f, LA.mapVector LA.conjugate g)
mkSigGate THG f g = (LA.mapVector (^2) f, id g)



shift :: ComplexSignal -> Int -> ComplexSignal
shift gate delay = LA.buildVector sz (builder delay)
    where
    sz = LA.dim gate
    builder d k = if any (<0) [(sz-1-d-k), (k+d)] then (getElem (k+d-(signum d)*sz)) else (getElem (k+d))
        where
        getElem = (LA.@>) gate


mkInitialGuess :: Int -> IO (LA.Vector Double)
mkInitialGuess sz = do
    stdgen <- R.getStdGen
    let randMags = R.randomRs (0, 1.0) stdgen
    let randAngs = R.randomRs (-pi, pi) stdgen
    return $ LA.buildVector (2*sz) (\k -> if k < sz then (randMags !! k) else (randAngs !! k))


flatPolar2Complex :: LA.Vector Double -> LA.Vector (LA.Complex Double)
flatPolar2Complex v = LA.buildVector sz (\k -> LA.mkPolar (mags LA.@> k) (angs LA.@> k))
    where
    sz = (LA.dim v) `quot` 2
    mags = LA.subVector 0 sz v
    angs = LA.subVector sz sz v
    
center :: ComplexSignal -> ComplexSignal
center f = shift f $ getDelay . getMaxInd $ f
    where
    sz = LA.dim f
    getMaxInd = LA.maxIndex . (LA.mapVector LA.magnitude)
    getDelay = (*(-1)) . ((-) (sz `quot` 2))



