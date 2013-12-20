
module Feature where

import Data.Matrix
import Data.Vector hiding ((++), drop)
import Graphics.Gloss hiding (Vector)

-- Feature has mu and covariance
-- size of Vector and Matrix are sadly not checked.
-- mu = <x,y,phi,rho>
data Feature = Feature {mu :: Vector Float, cov :: Matrix Float}
instance Show Feature where
	show f = "Feature" ++ drop 8 (show (mu f)) ++ "\n" ++ show (cov f)

data Point2  = Point2 {x :: Float, y :: Float}
	deriving (Show)

data Camera2 = Camera2 Point2 Float
	deriving (Show)

-- | Display pointcloud corresponding to a feature

