module Math.FROG.Types
( Nonlinearity(..)
, ComplexSignal
, Trace
) where

import qualified Numeric.LinearAlgebra as LA


type ComplexSignal = LA.Vector (LA.Complex Double)
type Trace = LA.Matrix Double


data Nonlinearity = PG | SHG | SD | THG
