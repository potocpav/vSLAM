
module Feature where

import Numeric.LinearAlgebra
--import Data.Random hiding (sample)
import Data.Random.Distribution.Normal
import Data.Random.Normal
import Data.Random (RVar)

import InternalMath

-- | Landmark ID, constituting a new type, because Integer arithmetics does not make sense.
newtype LID = LID Int deriving (Eq, Ord, Show)

-- | Inverse-depth 6D-parametrised landmark.
data Landmark = Landmark { lmk_id :: LID, mu :: Vector Double, cov :: Matrix Double }
instance Show Landmark where
	show l = "Landmark " ++ show (lmk_id l) ++ drop 8 (show (mu l)) ++ "\n" ++ show (cov l)
-- | Landmarks get compared only by their ID.
instance Eq Landmark where
	a == b = lmk_id a == lmk_id b
instance Ord Landmark where
	compare a b = compare (lmk_id a) (lmk_id b)

-- | Landmark projection, associated by lID with a maybe landmark.
data Feature = Feature  { lID :: LID, fProj :: (Double, Double) }

-- | Camera is parametrised by its position and a rotation matrix.
data Camera = Camera (Vector Double) (Matrix Double)
	deriving (Show)

-- | Currently not used. Probably could be deleted.
inverse2euclidean :: Landmark -> Landmark
inverse2euclidean (Landmark id mu cov) = Landmark id mu' cov' where
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
	randomStd seed = 6|> mkNormals seed
	
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
	
