
module Feature where

import Numeric.LinearAlgebra
--import Data.Random hiding (sample)
import Data.Random.Normal

import InternalMath

data Feature = Feature {eta :: Double, mu :: Vector Double, cov :: Matrix Double}
instance Show Feature where
	show (Feature eta mu cov) = "Feature " ++ show eta ++ drop 8 (show mu) ++ "\n" ++ show cov

type Measurement = (Double, Double)
-- data Point = Pt Double Double Double

-- | Camera is parametrised by its position and a rotation matrix.
data Camera = Camera (Vector Double) (Matrix Double)
	deriving (Show)



-- | Inverse depth to euclidean parametrization conversion
toXY :: Vector Double -> Vector Double
toXY i = (3|> [x,y,z]) + scale (1/rho) (euler2vec (theta,phi)) where
	[x,y,z,theta,phi,rho] = toList i


-- | Return a pseudo-random point from the feature distribution converted to
-- euclidean space
sample :: Feature -> Int -> Vector Double
sample (Feature _ mu cov) seed = toXY random6 where
	random6 = mu + cov <> randomStd seed
	randomStd seed = 6|> mkNormals seed
	
-- | Return a pseudo-random infinite list of points.
samples :: Feature -> Int -> [Vector Double]
samples feature seed = map (sample feature) [seed,seed+randomBigNumber..] where
	randomBigNumber = 234563
	
{-
-- | Return a random point from the feature distribution converted to
-- euclidean space
-- TODO: Delete if not needed.
sampleIO :: Feature -> IO (V.Vector Float)
sampleIO (Feature _ mu cov) = do
	normals <- normalsIO
	let randomStd = M.colVector (V.fromList $ take 4 normals)
	let random4 = M.getCol 1 $ M.colVector mu + cov * randomStd
	return $ toXY random4

-- | Take n random samples from Feature pdf
-- TODO: Delete if needed.
samplesIO :: Int -> Feature -> IO [V.Vector Float]
samplesIO n = sequence . take n . repeat . sampleIO

-}
