
module Camera where

import Numeric.LinearAlgebra

-- | Camera is parametrised by its position and a rotation matrix.
data ExactCamera = ExactCamera { cpos :: Vector Double, crot :: Matrix Double }
	deriving (Show)
-- | Sometimes we do not know the exact camera pose, this is the gaussian
-- uncertainty approximation in a 6D [pose, euler-angles] parametrization
data GaussianCamera = GaussianCamera { cmu :: Vector Double, ccov :: Matrix Double } 
	deriving (Show)

