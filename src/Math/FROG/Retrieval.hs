module Math.FROG.Retrieval
( retrieve
) where

import Numeric.LinearAlgebra (Vector, Matrix, fromList)

retrieve :: Matrix Double -> IO (Vector Double)
retrieve measurement = do
    return $ fromList [1::Double,2,3,4]
