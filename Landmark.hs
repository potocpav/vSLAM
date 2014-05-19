{-# LANGUAGE BangPatterns #-}

module Landmark where

import Numeric.LinearAlgebra
import Data.List (foldl')
import Data.Random (RVar)
import Data.Random.Normal
import Data.Random.Distribution.Normal
import qualified Data.ByteString as BS
import qualified Data.Set as S
import Data.Bits (xor, popCount)
import Data.Maybe (fromJust)

import InternalMath

-- | Landmark ID, constituting a new type, because Integer arithmetics do not make sense.
newtype LID = LID {fromLID :: Int} deriving (Eq, Ord, Show, Read)
-- |  Feature ID, constituting a new type, because Integer arithmetics do not make sense.
newtype FID = FID {fromFID :: Int} deriving (Eq, Ord, Show, Read)

type Descriptor = BS.ByteString


-- | Inverse-depth 6D-parametrised landmark.
data Landmark = Landmark 
		{ lid :: !LID
		, lmu :: !(Vector Double)
		, lcov :: !(Matrix Double)
		, ldescriptor :: !Descriptor
		, lhealth :: !Double
		} deriving (Show, Read)
-- | Landmarks get compared only by their ID.
instance Eq Landmark where
	a == b = lid a == lid b
instance Ord Landmark where
	compare a b = compare (lid a) (lid b)


-- | Landmark projection, called a Feature. It is sorted in the Set structure,
-- according to a globally-unique number fid. If it makes a new landmark, it gets
-- its ID.
data Feature = Feature 
		{ fid :: !FID				-- ^globally-unique number, useful for sorting in the Set structure, and initializing landmark id
		, flm :: !(Maybe Landmark)	-- ^if the feature gets associated to a landmark at some point, this is the point to tell the world
		, fpos :: !(Double, Double)	-- ^(theta, phi) pair in radians. (1,0) is positive-z, (0,1) is negative-y in camera coords.
		, response :: !Double		-- ^the bigger, the better corner was detected by Harris.
		, descriptor :: !Descriptor	-- ^Hamming distance is defined between the descriptors pairs.
		} deriving (Show, Read)
instance Eq Feature where
	a == b = fid a == fid b
instance Ord Feature where
	compare a b = compare (fid a) (fid b)
	
type Map = S.Set Landmark

fid2lid (FID i) = LID i

-- | Currently not used. Probably could be deleted. It can create 3D Euclidean
-- approximation to the 6D landmarks, to save some computing power. But the
-- 6D parametrization does have the first 3 rows and cols of covariance zero,
-- so the computing could be saved by instead taking advantage of that fact.
{-
inverse2euclidean :: Landmark -> Landmark
inverse2euclidean (Landmark id' mu cov d' lh') = Landmark id' mu' cov' d' lh' where
	mu' = toXYZ mu
	[_,_,_,theta,phi,rho] = toList mu
	cov' = jacob <> cov <> trans jacob
	jacob = (3><6) 
		[ 1, 0, 0,    cos phi * cos theta / rho, (-sin phi) * sin theta / rho, (-cos phi) * sin theta / (rho*rho)
		, 0, 1, 0,             (-sin phi) / rho,             (-cos phi) / rho,              (sin phi) / (rho*rho)
		, 0, 0, 1, (-cos phi) * sin theta / rho, (-sin phi) * cos theta / rho, (-cos phi) * cos theta / (rho*rho)
		]
-}

-- | The number of different bits between the descriptor.
-- Or, the number of set bits after a XOR operation.
hammingDist :: Descriptor -> Descriptor -> Int
hammingDist d1 d2 = foldl' (\ !a !w -> popCount w + a) 0 (BS.zipWith xor d1 d2)


-- | Return a pseudo-random point from the landmark distribution converted to
-- euclidean space
sample :: Landmark -> Int -> Vector Double
sample l seed = toXYZ random6 where
	random6 = (lmu l) + (lcov l) <> randomStd seed
	randomStd s = 6|> mkNormals s
	
-- | Return a pseudo-random infinite list of samples.
samples :: Landmark -> Int -> [Vector Double]
samples landmark seed = map (sample landmark) [seed,seed+randomBigNumber..] where
	randomBigNumber = 234563
	
-- | Return a pseudo-random point from the landmark distribution converted to
-- euclidean space
randomSample :: Landmark -> RVar (Vector Double)
randomSample l = do
	rndVec <- sequence (repeat stdNormal)
	return . toXYZ $ (lmu l) + (lcov l) <> (6|>rndVec)
	
