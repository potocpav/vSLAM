

module Simulate (trueMap, measurement, camTransition) where

import Data.Maybe (catMaybes)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util ((&))
import Data.Random (RVar, runRVar)
import Data.Random.Distribution.Normal
import Data.Random.Source.DevRandom

import Landmark
import Linear
import Camera
import InternalMath

-- | Create a map, consisting of some predefined landmarks.
trueMap :: [(LID, V3 Double)]
trueMap =
	[ (LID 1, V3 5 0 5)
	, (LID 2, V3 5 0 0)
	, (LID 3, V3 2 0 (-5))
	, (LID 4, V3 10 (-3) 5)
	, (LID 5, V3 5 3 10)
	, (LID 6, V3 (-5) 0 0)
	, (LID 7, V3 (5) (-3) (-10))
	, (LID 8, V3 (-50) 20 (-50))
	]

{- [V3 5 0 0, V3 0 0 5, V3 0 1 5, V3 5 5 5, V3 (-3) (-1.5) (-2) {-, V3 (-3000) (-3000) 3000-}] -}

measurePoint :: ExactCamera -> (LID, V3 Double) -> RVar Feature
measurePoint (ExactCamera cp cr) (lid, (V3 x y z)) = do
	let (phi,theta) = vec2euler $ trans cr <> ((3|> [x,y,z]) - cp)
	phi' <- stdNormal
	theta' <- stdNormal
	return $ Feature lid (phi+phi'*0.01, theta+theta'*0.01)

-- | Measure the given landmarks, with an infinite precision.
measurement' :: ExactCamera -> [(LID, V3 Double)] -> RVar [Feature]
measurement' cam = sequence . map (measurePoint cam)

-- | Measure the landmarks, defined in this file, with a finite precission (additive gaussian noise).
measurement :: ExactCamera -> IO [Feature]
measurement cam = runRVar (measurement' cam trueMap) DevURandom
		

-- | Transform the known camera position according to a supplied probabilistic
-- function (that takes the previous pose as a 6-vector.
proposal :: (Vector Double -> GaussianCamera) -> ExactCamera -> GaussianCamera
proposal f (ExactCamera cpos crot) = f (cpos & rotmat2euler crot)


camTransition :: [ExactCamera] -> ExactCamera -> GaussianCamera
camTransition [] _ = undefined
camTransition (ExactCamera cpos crot : []) _ = GaussianCamera (6|> repeat 0) ((6><6) (repeat 0))
camTransition (ExactCamera cp cr : ExactCamera cp' cr': _) (ExactCamera ccp ccr) = let 
	dp = cp - cp'
	in GaussianCamera ((dp*0 + ccp) & rotmat2euler ccr) (diag (6|> [0.1,0.1,0.01,0.1,0.01,0.01]))



