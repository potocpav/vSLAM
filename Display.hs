
module Display (display) where

import Feature
import EKF2D

import Data.Matrix hiding (fromList, (!), trace)
import Data.Vector hiding ((++), drop, take, map, update)

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

drawFeature :: Feature -> Picture
drawFeature f@(Feature mu cov) = pictures $ lines ++ shownPoints where
	lines = [Color red $ Line [(mu!0, mu!1), (mu!0 + sin(mu!2)/ (mu!3), mu!1 + cos(mu!2) / (mu!3))]]
	points = samples f 8000 -- this number is seed
	shownPoints = (\v -> Translate (v!0) (v!1) . Color (if mu!3 > 0 then black else red) $ Circle 0.02) `fmap` take 1000 points -- this number is nr. of points

main = do
	let f1 = initialize (Camera2 (Point2 0 0) 0) (0, 0.05)
	let f2 = update f1 (Camera2 (Point2 (-1) (0)) 0.1) (-0.0, 0.05)
	print f1
	print f2
	
	animate (InWindow "My Window" (w, h) (10, 10)) white (picture f1) where
		picture f t = project $  pictures 
			[ Circle 1
			, drawFeature $ update f (Camera2 (Point2 (-2) (2)) (t/10)) (-0.0, 0.15)
			, Line [(sin(t/10-pi/4)*5-2,cos(t/10-pi/4)*5+2),(-2,2),(sin(t/10+pi/4)*5-2,cos(t/10+pi/4)*5+2)] ]
		w = 600; h = 600
		project = Scale (fromIntegral w/2) (fromIntegral h/2)
	
