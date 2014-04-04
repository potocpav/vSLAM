
module Camera where

import Numeric.LinearAlgebra

-- | Camera is parametrised by its position and a rotation matrix.
data ExactCamera = ExactCamera { cpos :: Vector Double, crot :: Matrix Double }
	deriving (Show)
-- | Sometimes we do not know the exact camera pose, this is the gaussian
-- uncertainty approximation in a 6D [pose, euler-angles] parametrization
data GaussianCamera = GaussianCamera { cmu :: Vector Double, ccov :: Matrix Double } 
	deriving (Show)


-- | Return one possible triplet of euler angles, corresponding to the given
-- rotation matrix
rotmat2euler :: Matrix Double -> Vector Double
rotmat2euler m = case abs (m @@> (1,2)) of
	1 -> let
		alpha = 0
		beta = -asin (m @@> (1,2))
		gamma = atan2 (m @@> (0,1) * m @@> (1,2)) (m @@> (0,0) * m@@> (1,2))
			in 3 |> [alpha, beta, gamma]
	otherwise -> let
		alpha = -atan2 (m @@> (0,2) / cos beta) (m @@> (2,2) / cos beta)
		beta = -asin (m @@> (1,2))
		gamma = -atan2 (m @@> (1,0) / cos beta) (m @@> (1,1) / cos beta)
			in 3 |> [alpha, beta, gamma]


-- | Convert euler angles to the rotation matrix in 3D.
euler2rotmat :: Vector Double -> Matrix Double
euler2rotmat ea = (3><3)	
	[  sa*sb*sg + ca*cg, -cg*sa*sb + ca*sg, -cb*sa
	,            -cb*sg,             cb*cg,    -sb
	, -ca*sb*sg + cg*sa,  ca*cg*sb + sa*sg,  ca*cb ] where
	[alpha, beta, gamma] = toList ea
	sa = sin alpha; sb = sin beta; sg = sin gamma;
	ca = cos alpha; cb = cos beta; cg = cos gamma;
