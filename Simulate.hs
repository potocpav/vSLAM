

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
	, (LID 3, V3 0 0 5)
	, (LID 4, V3 10 (-3) 5)
	, (LID 5, V3 5 3 10)
	, (LID 6, V3 (-5) 0 0)
	, (LID 7, V3 (5) (-3) (-10))
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



-- | The first ExactCamera argument is the 'true' ExactCamera position, that the oracle
-- told us. The ExactCameras are the lists of positions in time (t:t-1:t-2:...).
-- The 'true' ExactCamera input is one step more advanced (one item longer list),
-- than the second argument.
camTransition :: ExactCamera -> GaussianCamera
camTransition :: [ExactCamera] -> ExactCamera -> RVar ExactCamera
camTransition [] _ = undefined
camTransition (c1:[]) _ = return c1
camTransition (ExactCamera cp2 cr2:ExactCamera cp1 cr1:_) (ExactCamera cp cr) = do
	[xdev,ydev,zdev,wdev] <- sequence . take 4 $ repeat stdNormal
	let posdev = 3|> [xdev*0.1,ydev*0.0,zdev*0.1]
	let thetadev = wdev * 0.1
	return $ ExactCamera (cp + movement*0 + posdev) (cr <> rotation <> rotateYmat thetadev) where
		(movement, rotation) = (cp2 - cp1, cr2 <> trans cr1)


