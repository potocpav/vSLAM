
module Feature where

import Numeric.LinearAlgebra
--import Data.Random hiding (sample)
import Data.Random.Distribution.Normal
import Data.Random.Normal
import Data.Random (RVar)

import InternalMath

data Feature = Feature { eta :: Double, mu :: Vector Double, cov :: Matrix Double }
instance Show Feature where
	show f = "Feature " ++ show (eta f) ++ drop 8 (show (mu f)) ++ "\n" ++ show (cov f)

type Measurement = (Double, Double)
-- data Point = Pt Double Double Double

-- | Camera is parametrised by its position and a rotation matrix.
data Camera = Camera (Vector Double) (Matrix Double)
	deriving (Show)

inverse2euclidean :: Feature -> Feature
inverse2euclidean (Feature eta mu cov) = Feature eta mu' cov' where
	mu' = toXYZ mu
	[_,_,_,theta,phi,rho] = toList mu
	cov' = jacob <> cov <> trans jacob
	jacob = (3><6) 
		[ 1, 0, 0,    cos phi * cos theta / rho, (-sin phi) * sin theta / rho, (-cos phi) * sin theta / (rho*rho)
		, 0, 1, 0,             (-sin phi) / rho,             (-cos phi) / rho,              (sin phi) / (rho*rho)
		, 0, 0, 1, (-cos phi) * sin theta / rho, (-sin phi) * cos theta / rho, (-cos phi) * cos theta / (rho*rho)
		]


-- | Return a pseudo-random point from the feature distribution converted to
-- euclidean space
sample :: Feature -> Int -> Vector Double
sample (Feature _ mu cov) seed = toXYZ random6 where
	random6 = mu + cov <> randomStd seed
	randomStd seed = 6|> mkNormals seed
	
-- | Return a pseudo-random infinite list of points.
samples :: Feature -> Int -> [Vector Double]
samples feature seed = map (sample feature) [seed,seed+randomBigNumber..] where
	randomBigNumber = 234563
	
-- | Return a pseudo-random point from the feature distribution converted to
-- euclidean space
randomSample :: Vector Double -> Matrix Double -> RVar (Vector Double)
randomSample mu cov = do
	rndVec <- sequence (repeat stdNormal)
	return . toXYZ $ mu + cov <> (6|>rndVec)
	
