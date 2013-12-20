
module Feature where

import Data.Matrix
import Graphics.Gloss

data Point2  = Point2 {x :: Float, y :: Float}
	deriving (Show)

-- | Inverse depth parametrization
-- (cx, cy) .. camera center
-- phi      .. angle of ray
-- id       .. inverse depth
data Feature4 = Feature4 Point2 Float Float
	deriving (Show)

type Cov4 = Matrix Float

data Camera2 = Camera2 Point2 Float
	deriving (Show)

-- | Display pointcloud corresponding to a feature

