
module InternalMath where

import Numeric.LinearAlgebra

-- | Un-normalized 3-vec parametrization to azimuth-elevation pair
vec2euler :: Vector Double -> (Double, Double)
vec2euler v = (atan2 x z, atan2 (-y) (sqrt(x*x + z*z))) where
	[x,y,z] = toList v

-- | Azimuth-elevation parametrization to normalized 3-vec
euler2vec :: (Double, Double) -> Vector Double
euler2vec (theta, phi) = 3 |> [cos phi * sin theta, -sin phi, cos phi * cos theta]


