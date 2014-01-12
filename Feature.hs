
module Feature where

import qualified Data.Matrix as M
import qualified Data.Vector as V

-- Feature has mu and covariance
-- size of Vector and Matrix are sadly not checked.
-- mu = <x,y,phi,rho>
data Feature = Feature {mu :: V.Vector Float, cov :: M.Matrix Float}
instance Show Feature where
	show f = "Feature" ++ drop 8 (show (mu f)) ++ "\n" ++ show (cov f)

type Point = (Float, Float)

data Camera = Camera Point Float
	deriving (Show)

-- | Inverse depth to euclidean parametrization conversion
toXY :: V.Vector Float -> V.Vector Float
toXY v = V.fromList [(v!0) + sin(v!2) / (v!3), (v!1) + cos(v!2) / (v!3)] where
	(!) = (V.!)
