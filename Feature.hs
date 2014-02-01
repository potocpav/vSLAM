
module Feature where

import qualified Data.Matrix as M
import qualified Data.Vector as V

--import Data.Random hiding (sample)
import Data.Random.Normal

-- Feature has mu and covariance
-- size of Vector and Matrix are sadly not checked.
-- mu = <x,y,phi,rho>
data Feature = Feature {eta :: Float, mu :: V.Vector Float, cov :: M.Matrix Float}
instance Show Feature where
	show (Feature eta mu cov) = "Feature " ++ show eta ++ "\n" ++ drop 8 (show mu) ++ "\n" ++ show cov

type Measurement = Float
type Point = (Float, Float)

data Camera = Camera Point Float
	deriving (Show)

-- | Inverse depth to euclidean parametrization conversion
toXY :: V.Vector Float -> V.Vector Float
toXY v = V.fromList [(v!0) + sin(v!2) / (v!3), (v!1) + cos(v!2) / (v!3)] where
	(!) = (V.!)


-- | Return a random point from the feature distribution converted to
-- euclidean space
sample :: Feature -> Int -> V.Vector Float
sample (Feature _ mu cov) seed = toXY random4 where
	random4 = M.getCol 1 $ M.colVector mu + cov * randomStd seed
	randomStd seed = M.colVector $ V.fromList (take 4 $ mkNormals' (0,1) seed)

-- | Return a pseudo-random infinite list of points. List made of similar seeds are
-- themselves similar (seed different by n will result in n first samples different)
samples :: Feature -> Int -> [V.Vector Float]
samples feature seed = map (sample feature) [seed..]
