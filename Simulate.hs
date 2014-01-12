
module Simulate (initial, measurement) where

import Feature

import qualified Data.Set as S
import Data.Maybe (catMaybes, isJust, fromJust)

initial :: IO (S.Set Point)
initial = return $ S.fromList [(1,2),(-3,4),(0,5),(6,7)]

-- | perspective projection.
-- Near plane is at 1 and fov=90 degrees.
measure :: Camera -> Point -> Maybe Float
measure (Camera (cx, cy) phi) (px, py) = if y >= 1 && abs m <= 1
			then Just m else Nothing where
	m = x / y
	(x,y) = rotate (-phi) (px - cx) (py - cy)
	rotate a x y = (x*cos(a)+y*sin(a), -x*sin(a)+y*cos(a))

-- | Gets the set of all measured features. Their number is naturally smaller,
-- than the number of all landmarks: not all are in camera's FOV.
measurement :: Camera -> S.Set Point -> S.Set Float
measurement cam pts = S.map fromJust $ S.filter isJust (S.map (measure cam) pts)
