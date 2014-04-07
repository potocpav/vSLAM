
module InternalMath where

import Numeric.LinearAlgebra

-- for debugging
import System.IO.Unsafe (unsafePerformIO)
debug :: Show a => String -> a -> a
debug s a = a `seq` unsafePerformIO (print $ s ++ ": " ++ show a) `seq` a
infixr 1 `debug`

data Gauss = Gauss {gmu :: Vector Double, gcov :: Matrix Double}

-- | Un-normalized 3-vec parametrization to azimuth-elevation pair
vec2euler :: Vector Double -> (Double, Double)
vec2euler v = (atan2 x z, atan2 (-y) (sqrt(x*x + z*z))) where
	[x,y,z] = toList v

-- | Azimuth-elevation parametrization to normalized 3-vec
euler2vec :: (Double, Double) -> Vector Double
euler2vec (theta, phi) = 3 |> [cos phi * sin theta, -sin phi, cos phi * cos theta]


rotateYmat :: Double -> Matrix Double
rotateYmat a = (3><3) [  cos a, 0, sin a
					  ,      0, 1, 0
					  , -sin a, 0, cos a ]

-- | Inverse depth mean to euclidean parametrization conversion
toXYZ :: Vector Double -> Vector Double
toXYZ i = (3|> [x,y,z]) + scale (1/rho) (euler2vec (theta,phi)) where
	[x,y,z,theta,phi,rho] = toList i

cyclicDiff :: Double -> Double -> Double
cyclicDiff a b = a - b - (fromIntegral . round $ (a-b) / (2*pi)) * (2*pi)
