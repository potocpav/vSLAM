
module InternalMath where

import Numeric.LinearAlgebra

import Control.Exception

-- for debugging
import System.IO.Unsafe (unsafePerformIO)
debug :: Show a => String -> a -> a
debug s a = a `seq` unsafePerformIO (print $ s ++ ": " ++ show a) `seq` a
infixr 1 `debug`

data Gauss = Gauss {gmu :: Vector Double, gcov :: Matrix Double} deriving (Show)


-- | Compute the pdf of a multivariate normal distribution at the point m.
normalDensity :: Gauss -> Vector Double -> Double
normalDensity (Gauss mu cov) m = norm * exp e where 
	norm = (2*pi)**(-(fromIntegral$dim mu)/2) * (det cov)**(-0.5)
	e = (-0.5) * unpack (asRow(m-mu) <> inv cov <> asColumn(m-mu))
	unpack m = assert (rows m == 1 && cols m == 1) $ m @@> (0,0)
	
-- | Mahalanobis distance
mahalDist_sq :: Gauss -> Vector Double -> Double
mahalDist_sq (Gauss mu cov) x = (asRow (x-mu) <> inv cov <> asColumn(x-mu)) @@> (0,0)
	

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
