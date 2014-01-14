module Math.FROG.Tools
( mkTrace

, mkPGgate
, mkSHGgate
, mkSDgate
, mkTHGgate

, mkPGfield
, mkSHGfield
, mkSDfield
, mkTHGfield

, shift
) where

import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.GSL.Fourier as F
import qualified Foreign

type ComplexSignal = LA.Vector (LA.Complex Double)
type Trace = LA.Matrix Double
type SignalIntensity = LA.Vector Double

mkTrace :: ComplexSignal -> ComplexSignal -> Trace
mkTrace field gate = LA.fromColumns $ map atEachDelayApply [upper,upper-1..lower]
    where
    atEachDelayApply = (^2) . (LA.mapVector LA.magnitude) 
                             . (flip shift upper) 
                             .  F.fft 
                             . (*field) 
                             . (shift gate)
    sz = LA.dim field
    upper = (divideInt sz 2)
    lower = (-1) * (divideInt sz 2) + 1


mkPGgate :: ComplexSignal -> ComplexSignal
mkPGgate = LA.mapVector ((**2) . abs)

mkSHGgate :: ComplexSignal -> ComplexSignal
mkSHGgate = id

mkSDgate :: ComplexSignal -> ComplexSignal
mkSDgate = LA.mapVector LA.conjugate

mkTHGgate :: ComplexSignal -> ComplexSignal
mkTHGgate = id


mkPGfield :: ComplexSignal -> ComplexSignal
mkPGfield = id

mkSHGfield :: ComplexSignal -> ComplexSignal
mkSHGfield = id

mkSDfield :: ComplexSignal -> ComplexSignal
mkSDfield = LA.mapVector (**2)

mkTHGfield :: ComplexSignal -> ComplexSignal
mkTHGfield = LA.mapVector (**2)


shift :: ComplexSignal -> Int -> ComplexSignal
shift gate delay = LA.buildVector sz (builder delay)
    where
    sz = LA.dim gate
    builder d k = if k > (sz-1-d) then (getElem (k+d-sz)) else (getElem (k+d))
        where
        getElem = (LA.@>) gate

divideInt :: Int -> Int -> Int
divideInt n d = truncate ((fromIntegral n) / (fromIntegral d))
