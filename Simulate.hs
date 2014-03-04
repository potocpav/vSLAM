

module Simulate (landmarks, measurePoint, measurement) where

import Data.Maybe (catMaybes)
import Numeric.LinearAlgebra
import Feature
import Linear
import InternalMath

-- | Create a map, consisting of some predefined landmarks.
landmarks :: [V3 Double]
landmarks = [V3 5 0 0, V3 0 0 5, V3 0 5 0, V3 5 5 5]

measurePoint :: Camera -> V3 Double -> Maybe Measurement
measurePoint (Camera cp cr) (V3 x y z) = Just m where
	m = vec2euler $ trans cr <> ((3|> [x,y,z]) - cp)

measurement' :: Camera -> [V3 Double] -> [Measurement]
measurement' cam = catMaybes . map (measurePoint cam)

measurement :: Camera -> [Measurement]
measurement = flip measurement' $ landmarks
