{-|
Module      : Camera
License     : WTFPL
Maintainer  : Pavel Potocek <pavelpotocek@gmail.com>

This package provides the 'ExactCamera' and 'GaussianCamera' data types together
with some functions manipulating the cameras..
-}

module Camera where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util ((&), (!), (#))
import InternalMath


-- | 'ExactCamera' is parametrised by its position and a rotation matrix.
data ExactCamera = ExactCamera { cpos :: Vector Double, crot :: Matrix Double }
	deriving (Show)
	
-- | Sometimes we do not know the exact camera pose. This is the gaussian
-- uncertainty approximation in a 6D [pose, Tait-Bryan y-x'-z"] parametrization
data GaussianCamera = GaussianCamera { cmu :: Vector Double, ccov :: Matrix Double } 
	deriving (Show)


-- | Return an 'ExactCamera' from the 'GaussianCamera' mean value, disregarding
-- covariance
gauss2exact :: GaussianCamera -> ExactCamera
gauss2exact cam = ExactCamera (3|> [x,y,z]) (euler2rotmat (3|> [a,b,g])) where
	[x,y,z,a,b,g] = toList (cmu cam)


-- | Return one possible triplet of euler angles, corresponding to the given
-- rotation matrix
rotmat2euler :: Matrix Double -> Vector Double
rotmat2euler m = case abs (m @@> (1,2)) of
	1 -> let
		alpha = debug "zero" 0
		beta = -asin (m @@> (1,2))
		gamma = atan2 (m @@> (0,1)) (m @@> (0,0))
			in 3 |> [alpha, beta, gamma]
	otherwise -> let
		alpha = -atan2 (m @@> (0,2) / cos beta) (m @@> (2,2) / cos beta)
		beta = -asin (m @@> (1,2))
		gamma = -atan2 (m @@> (1,0) / cos beta) (m @@> (1,1) / cos beta)
			in 3 |> [alpha, beta, gamma]


-- | Convert Tait-Bryan y-x'-z'' angles to the rotation matrix in 3D.
euler2rotmat :: Vector Double -> Matrix Double
euler2rotmat ea = (3><3)	
	[  sa*sb*sg + ca*cg, -cg*sa*sb + ca*sg, -cb*sa
	,            -cb*sg,             cb*cg,    -sb
	, -ca*sb*sg + cg*sa,  ca*cg*sb + sa*sg,  ca*cb ] where
	[alpha, beta, gamma] = toList ea
	sa = sin alpha; sb = sin beta; sg = sin gamma;
	ca = cos alpha; cb = cos beta; cg = cos gamma;


averageCams :: [ExactCamera] -> ExactCamera
averageCams cs = ExactCamera (3|> [x,y,z]) (euler2rotmat (3|> [a,b,g])) where
	[x,y,z,a,b,g] = toList $ sum [cpos c & rotmat2euler (crot c) | c <- cs] / fromIntegral (length cs)


-- | Camera to an OpenGL-like 4x4 transformation matrix
camToTF :: ExactCamera -> Matrix Double
camToTF (ExactCamera cp cr) = (cr ! asColumn cp) # (1><4) [0,0,0,1]


