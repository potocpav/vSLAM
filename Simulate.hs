

module Simulate (landmarks, measurement, camTransition) where

import Data.Maybe (catMaybes)
import Numeric.LinearAlgebra
import Data.Random (RVar, runRVar)
import Data.Random.Distribution.Normal
import Data.Random.Source.DevRandom
import Feature
import Linear
import InternalMath

-- | Create a map, consisting of some predefined landmarks.
landmarks :: [V3 Double]
landmarks = {- [V3 (0) (0) (5) , V3 (0) (-5) (5)] -}
	[ V3 ( -2.959143) (2.953925) (-11.797375)
	, V3 (-5.079667) (-3.711716) (3.123566)
	, V3 (-5.767245) (6.669867) (5.087771)
	, V3 (4.164903) (6.146811) (-11.049625)
	, V3 (2.552198) (-6.216892) (3.876086)
	, V3 (-1.122887) (-7.755002) (-2.579167)
	, V3 (1.517360) (-0.498342) (12.196419)
	, V3 (4.150074) (4.171879) (-1.110384)
	, V3 (-7.215903) (8.964795) (-4.970025)
	, V3 (-2.311995) (-1.378517) (10.119724)
	, V3 (-0.090692) (1.509916) (-7.294614)
	, V3 (-5.818216) (-0.496758) (-0.524454)
	, V3 (-0.579933) (-5.355669) (9.207645)
	, V3 (-6.479491) (-9.870469) (-1.023540)
	, V3 (5.220224) (1.716936) (1.651066)
	, V3 (3.238513) (1.342487) (0.336019)
	, V3 (1.869136) (1.506089) (0.332289)
	, V3 (3.067332) (-2.391349) (-3.209567)
	, V3 (-0.813799) (-7.208116) (1.159423)
	, V3 (-2.377635) (3.052709) (-6.112094)
	]

{- [V3 5 0 0, V3 0 0 5, V3 0 1 5, V3 5 5 5, V3 (-3) (-1.5) (-2) {-, V3 (-3000) (-3000) 3000-}] -}

measurePoint :: Camera -> V3 Double -> RVar Measurement
measurePoint (Camera cp cr) (V3 x y z) = do
	let (phi,theta) = vec2euler $ trans cr <> ((3|> [x,y,z]) - cp)
	phi' <- stdNormal
	theta' <- stdNormal
	return (phi+phi'*0.01, theta+theta'*0.01)

-- | Measure the given landmarks, with an infinite precision.
measurement' :: Camera -> [V3 Double] -> RVar [Measurement]
measurement' cam = sequence . map (measurePoint cam)

-- | Measure the landmarks, defined in this file, with a finite precission (additive gaussian noise).
measurement :: Camera -> IO [Measurement]
measurement cam = runRVar (measurement' cam landmarks) DevURandom
		


-- | The first camera argument is the 'true' camera position, that the oracle
-- told us. The cameras are the lists of positions in time (t:t-1:t-2:...).
-- The 'true' camera input is one step more advanced (one item longer list),
-- than the second argument.
camTransition :: [Camera] -> [Camera] -> RVar Camera
camTransition [] c = undefined
camTransition (c1:[]) [] = return c1
camTransition (Camera cp2 cr2:Camera cp1 cr1:_) ((Camera cp cr):_) = do
	[xdev,zdev,wdev] <- sequence . take 3 $ repeat stdNormal
	let posdev = 3|> [xdev*0.2,0,zdev*0.2]
	let thetadev = wdev * 0.0
	return $ Camera (cp + movement*0 + posdev) (cr {- <> rotation -} <> rotateYmat thetadev) where
		(movement, rotation) = (cp2 - cp1, cr2 <> trans cr1)


