
module Display (display) where

import Feature
import EKF2D

import Data.Matrix hiding (fromList, (!), trace)
import Data.Vector hiding ((++), drop, take, map)

import Graphics.Gloss hiding (Vector)
import Data.Random.Normal
--import Debug.Trace (trace)

-- | Return a random point from the feature distribution, and converted to
-- euclidean space
sample :: Feature -> Int -> Vector Float
sample (Feature mu cov) seed = toXY random4 where
	random4 = getCol 1 $ colVector mu + cov * randomStd seed
	randomStd seed = colVector $ fromList (take 4 $ mkNormals' (0,1) seed)

-- | Return a random infinite list of points. Not so random, see implementation.
samples :: Feature -> Int -> [Vector Float]
samples feature seed = map (sample feature) [seed..]

main = do
	let feature = initialize (Camera2 (Point2 0 0) 0) (0, 0.05)
	print feature
	let points = samples feature 800
	let shownPoints = (\v -> Translate (v!0) (v!1) $ Circle 0.01) `fmap` take 1000 points
	
	display (InWindow "My Window" (w, h) (10, 10)) white (project $  pictures $ Circle 1:shownPoints) where
		w = 600; h = 600
		project = Scale (fromIntegral w/2) (fromIntegral h/2)
	
