

module Simulate (landmarks, measurement) where

import Data.Maybe (catMaybes)
import Numeric.LinearAlgebra
import Feature
import Linear
import InternalMath

-- | Create a map, consisting of some predefined landmarks.
landmarks :: [V3 Double]
landmarks = [V3 5 0 0, V3 0 0 5, V3 0 (-1) 5, V3 5 5 5, V3 (-3) (-1.5) (-2), V3 (-3000) (-3000) 3000]

measurePoint :: Camera -> V3 Double -> Maybe Measurement
measurePoint (Camera cp cr) (V3 x y z) = Just m where
	m = vec2euler $ trans cr <> ((3|> [x,y,z]) - cp)

-- | Measure the given landmarks, with an infinite precision.
measurement' :: Camera -> [V3 Double] -> [Measurement]
measurement' cam = catMaybes . map (measurePoint cam)

-- | Measure the landmarks, defined in this file, with a finite precission (additive gaussian noise).
measurement :: Camera -> IO [Measurement]
measurement cam = return $ measurement' cam (filter (\l -> distSq l cam >= 0) landmarks) where
	distSq (V3 x1 y1 z1) (Camera c _) = (x1-c@>0)^^2 + (y1-c@>1)^^2 + (z1-c@>2)^^2
