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

-- | Landmark ID. Because integer arithmetics do not make sense in this case.
newtype LID = LID {fromLID :: Int} deriving (Eq, Ord, Show, Read)
-- | Feature ID. Because integer arithmetics do not make sense in this case.
newtype FID = FID {fromFID :: Int} deriving (Eq, Ord, Show, Read)

fid2lid (FID i) = LID i

-- | Feature descriptor. It needs to be a binary descriptor for the algorithms
-- to work.
type Descriptor = BS.ByteString


-- | Inverse-depth parametrised 6D landmark.
data Landmark = Landmark 
		{ lid :: !LID
		, lmu :: !(Vector Double)
		, lcov :: !(Matrix Double)
		, ldescriptor :: !Descriptor
		, lhealth :: !Double
		} deriving (Show, Read)
-- | Landmarks get compared by their ID.
instance Eq Landmark where
	a == b = lid a == lid b
instance Ord Landmark where
	compare a b = compare (lid a) (lid b)


-- | Landmark projection called a Feature. It is sorted in the Set structure
-- according to a globally-unique number fid. If it makes a new landmark, it gets
-- its ID.
data Feature = Feature 
		{ fid :: !FID				-- ^A globally-unique number useful for sorting in the Set structure, and the landmark id initialization
		, flm :: !(Maybe Landmark)	-- ^If the feature gets associated to a landmark at some point, this is the point to tell the world
		, fpos :: !(Double, Double)	-- ^(theta, phi) position in radians. (1,0) is a positive-z, (0,1) is a negative-y in camera coords.
		, response :: !Double		-- ^response strength (ORB uses Harris)
		, descriptor :: !Descriptor	-- ^Hamming distance-measurable descriptor with binary letters
		} deriving (Show, Read)
-- | Features are compared by their IDs
instance Eq Feature where
	a == b = fid a == fid b
instance Ord Feature where
	compare a b = compare (fid a) (fid b)


type Map = S.Set Landmark


-- | The number of different bits between the descriptors.
-- Or, the number of set bits after a XOR operation.
-- It may space-leak. Investigate!
hammingDist :: Descriptor -> Descriptor -> Int
hammingDist d1 d2 = foldl' (\ !a !w -> popCount w + a) 0 (BS.zipWith xor d1 d2)


-- | Take a pseudo-random point from the landmark distribution and convert it
-- to the euclidean space
sample :: Landmark -> Int -> Vector Double
sample l seed = toXYZ random6 where
	random6 = (lmu l) + (lcov l) <> randomStd seed
	randomStd s = 6|> mkNormals s
	
	
-- | Return a pseudo-random infinite list of samples.
samples :: Landmark -> Int -> [Vector Double]
samples landmark seed = map (sample landmark) [seed,seed+randomBigNumber..] where
	randomBigNumber = 234563

