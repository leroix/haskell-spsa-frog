module Math.FROG.Tools where
( mkTrace

, mkPGgate
, mkSHGgate
, mkSDgate
, mkTHGgate

, mkPGfield
, mkSHGfield
, mkSDfield
, mkTHGfield
) where

import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.GSL.Fourier as F

type ComplexSignal = LA.Vector (LA.Complex Double)
type Trace = LA.Matrix Double
type SignalIntensity = LA.Vector Double

mkTrace :: ComplexSignal -> LA.Vector a -> Trace
mkTrace field gate = LA.fromColums $ map atEachDelayDo [upper,upper-1..lower]
    where
    atEachDelayDo = (**2) . LA.magnitude . F.fft . (*field) . (shift gate)
    sz = LA.dim field
    upper = (divideInt sz 2)
    lower = (-1) * (divideInt sz 2) + 1


mkPGgate :: ComplexSignal -> SignalIntensity
mkPGgate = LA.mapVector ((**2) . LA.magnitude)

mkSHGgate :: ComplexSignal -> ComplexSignal
mkSHGgate = id

mkSDgate :: ComplexSignal -> ComplexSignal
mkSDgate = LA.mapVector C.conjugate

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


shift :: LA.Vector a -> Int -> LA.Vector a
shift gate delay = LA.buildVector sz (builder delay)
    where
    sz = LA.dim gate
    builder d k = if k > (sz-1-d) then (getElem (k+d-sz)) else (getElem (k+d))
        where
        getElem = (LA.@>) gate

divideInt :: Int -> Int -> Int
divideInt n d = truncate ((fromIntegral n) / (fromIntegral d))
