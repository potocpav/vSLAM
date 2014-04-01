

module Simulate (trueMap, measurement, camTransition) where

import Data.Maybe (catMaybes)
import Numeric.LinearAlgebra
import Data.Random (RVar, runRVar)
import Data.Random.Distribution.Normal
import Data.Random.Source.DevRandom
import Feature
import Linear
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

measurePoint :: Camera -> (LID, V3 Double) -> RVar Feature
measurePoint (Camera cp cr) (lid, (V3 x y z)) = do
	let (phi,theta) = vec2euler $ trans cr <> ((3|> [x,y,z]) - cp)
	phi' <- stdNormal
	theta' <- stdNormal
	return $ Feature lid (phi+phi'*0.01, theta+theta'*0.01)

-- | Measure the given landmarks, with an infinite precision.
measurement' :: Camera -> [(LID, V3 Double)] -> RVar [Feature]
measurement' cam = sequence . map (measurePoint cam)

-- | Measure the landmarks, defined in this file, with a finite precission (additive gaussian noise).
measurement :: Camera -> IO [Feature]
measurement cam = runRVar (measurement' cam trueMap) DevURandom
		


-- | The first camera argument is the 'true' camera position, that the oracle
-- told us. The cameras are the lists of positions in time (t:t-1:t-2:...).
-- The 'true' camera input is one step more advanced (one item longer list),
-- than the second argument.
camTransition :: [Camera] -> Camera -> RVar Camera
camTransition [] _ = undefined
camTransition (c1:[]) _ = return c1
camTransition (Camera cp2 cr2:Camera cp1 cr1:_) (Camera cp cr) = do
	[xdev,zdev,wdev] <- sequence . take 3 $ repeat stdNormal
	let posdev = 3|> [xdev*0.1,0,zdev*0.1]
	let thetadev = wdev * 0.1
	return $ Camera (cp + movement*0 + posdev) (cr <> rotation <> rotateYmat thetadev) where
		(movement, rotation) = (cp2 - cp1, cr2 <> trans cr1)


