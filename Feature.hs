
module Feature where

import Data.Matrix (Matrix)
import Data.Vector (Vector, (!), fromList)

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

-- | Inverse depth to euclidean parametrization conversion
toXY :: Vector Float -> Vector Float
toXY v = fromList [(v!0)+sin(v!2)/(v!3), (v!1)+cos(v!2)/(v!3)]
