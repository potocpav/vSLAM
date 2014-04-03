
module Landmark where

import Numeric.LinearAlgebra
import Data.Random (RVar)
import Data.Random.Normal
import Data.Random.Distribution.Normal

import InternalMath

-- | Landmark ID, constituting a new type, because Integer arithmetics does not make sense.
newtype LID = LID Int deriving (Eq, Ord, Show)


-- | Inverse-depth 6D-parametrised landmark.
data Landmark = Landmark { lid :: LID, lmu :: Vector Double, lcov :: Matrix Double }
instance Show Landmark where
	show l = "Landmark " ++ show (lid l) ++ drop 8 (show (lmu l)) ++ "\n" ++ show (lcov l)
-- | Landmarks get compared only by their ID.
instance Eq Landmark where
	a == b = lid a == lid b
instance Ord Landmark where
	compare a b = compare (lid a) (lid b)

-- | Landmark projection, associated by lID with a landmark (which need not exist yet).
data Feature = Feature  { fid :: LID, fProj :: (Double, Double) }
	

-- | Currently not used. Probably could be deleted. It can create 3D Euclidean
-- approximation to the 6D landmarks, to save some computing power. But the
-- 6D parametrization does have the first 3 rows and cols of covariance zero,
-- so the computing could be saved by instead taking advantage of that fact.
inverse2euclidean :: Landmark -> Landmark
inverse2euclidean (Landmark id' mu cov) = Landmark id' mu' cov' where
	mu' = toXYZ mu
	[_,_,_,theta,phi,rho] = toList mu
	cov' = jacob <> cov <> trans jacob
	jacob = (3><6) 
		[ 1, 0, 0,    cos phi * cos theta / rho, (-sin phi) * sin theta / rho, (-cos phi) * sin theta / (rho*rho)
		, 0, 1, 0,             (-sin phi) / rho,             (-cos phi) / rho,              (sin phi) / (rho*rho)
		, 0, 0, 1, (-cos phi) * sin theta / rho, (-sin phi) * cos theta / rho, (-cos phi) * cos theta / (rho*rho)
		]


-- | Return a pseudo-random point from the landmark distribution converted to
-- euclidean space
sample :: Landmark -> Int -> Vector Double
sample (Landmark _ mu cov) seed = toXYZ random6 where
	random6 = mu + cov <> randomStd seed
	randomStd s = 6|> mkNormals s
	
-- | Return a pseudo-random infinite list of samples.
samples :: Landmark -> Int -> [Vector Double]
samples feature seed = map (sample feature) [seed,seed+randomBigNumber..] where
	randomBigNumber = 234563
	
-- | Return a pseudo-random point from the landmark distribution converted to
-- euclidean space
randomSample :: Landmark -> RVar (Vector Double)
randomSample (Landmark _ mu cov) = do
	rndVec <- sequence (repeat stdNormal)
	return . toXYZ $ mu + cov <> (6|>rndVec)
	
