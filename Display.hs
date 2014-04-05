{-# OPTIONS_GHC -Wall #-}

-- module Main ( main ) where

import Linear
import SpatialMath ( Euler(..), rotateXyzAboutY, rotVecByEulerB2A, rotateXyzAboutX )
import Graphics.X11 ( initThreads )
import Vis
import Graphics.UI.GLUT hiding ( Plane, Sphere, Points, Cube, Line, motionCallback, samples, initialize )
import qualified Data.Set as Set
import Control.Monad ( when )
import Data.Random hiding (sample)
import Data.Random.Source.DevRandom
import Numeric.LinearAlgebra

import InternalMath
import Landmark
import Camera
import Simulate
import FastSLAM2

ts :: Double
ts = 0.01


data ObserverState = Running (V3 Double) (V3 Double) (Euler Double)
data InputState = Input { keySet :: Set.Set Key, lastMousePos :: Maybe (GLint, GLint), spacePressed :: Bool, xPressed :: Bool }
-- | The true camera position sequence, measurement history, and a particle set.
data SLAMState = SLAM { cameras :: [ExactCamera], measurements :: [[Feature]], particles :: [(ExactCamera, Map)] }

data GameState = GameState { observer :: ObserverState
                           , input :: InputState
                           , slam :: SLAMState 
                           }

toVertex :: (Real a, Fractional b) => V3 a -> Vertex3 b
toVertex xyz = (\(V3 x y z) -> Vertex3 x y z) $ fmap realToFrac xyz

setCamera :: ObserverState -> IO ()
setCamera (Running (V3 x y z) _ euler) = lookAt (toVertex xyz0) (toVertex target) (Vector3 0 (-1) 0)
	where
		xyz0 = V3 x y z
		target = xyz0 + rotateXyzAboutY (rotateXyzAboutX (rotVecByEulerB2A euler (V3 1 0 0)) (-pi/2)) (-pi/2)

simfun :: Float -> GameState -> IO GameState
simfun _ (GameState (Running pos _ euler0@(Euler yaw _ _)) input' (SLAM cams mss ps)) = do
	Size x y <- get windowSize
	let 
		x' = (fromIntegral x) `div` 2
		y' = (fromIntegral y) `div` 2
	meas <- measurement (head cams)
	let measure_history = if spacePressed input' then meas:mss else mss
	-- | Run the FastSLAM routine
	ps' <- if not (spacePressed input') then return ps else
		(flip runRVar) DevURandom $ filterUpdate
				ps
				(camTransition cams)
				meas
	
	-- | if space pressed, then copy the first camera. This has to be -after-
	-- the Fast routine.
	let cams' = if spacePressed input' then head cams : cams else cams
	
	{- 'x' key behavior
	when (xPressed input') $ do
		let lastCam = head $ (\(_,a,_) -> a) (head ps)
		print lastCam
		meas' <- measurement lastCam
		newParticle <- (flip runRVar) DevURandom $ (\ms psl f -> sequence (map (\p -> updateParticle0 ms p f) psl))
				(meas':mss)
				ps
				(camTransition cams)
		--- sequence $ map (putStrLn.show) ((\(_,_,t)->t) (head ps))
		print $ (\(w,_,_) -> w) (head newParticle)
		return ()
	-}
	
	when (Just (x',y') /= lastMousePos input') (pointerPosition $= (Position x' y'))

	return $ GameState 
		(Running (pos + (ts *^ v)) v euler0) 
		input' { lastMousePos = Just (x',y'), spacePressed = False, xPressed = False }
		(SLAM ((newcam $ head cams'):tail cams') measure_history ps') where
			keyPressed k = Set.member (Char k) (keySet input')
			newcam (ExactCamera cp cr) = ExactCamera (cp + cr <> (3|> [right-left,0,up-down])) (cr <> rot) where
					rot = rotateYmat ((rturn-lturn)*ts)
					up    = if keyPressed 'u' then ts else 0
					down  = if keyPressed 'e' then ts else 0
					left  = if keyPressed 'n' then ts else 0
					right = if keyPressed 'i' then ts else 0
					lturn = if keyPressed 'l' then 2 else 0
					rturn = if keyPressed 'y' then 2 else 0
			v = rotateXyzAboutY (V3 (d-a) (dn-up) (w-s)) yaw where
					w = if keyPressed 'w' then 3 else 0
					a = if keyPressed 'a' then 3 else 0
					s = if keyPressed 'r' then 3 else 0
					d = if keyPressed 's' then 3 else 0
					up = if keyPressed 'p' then 3 else 0
					dn = if keyPressed 't' then 3 else 0

keyMouseCallback :: GameState -> Key -> KeyState -> Modifiers -> Position -> GameState
keyMouseCallback state0 key keystate _ _
	| keystate == Down = state0 {input = (input state0) {keySet = Set.insert key (keySet $ input state0), 
		spacePressed = (key == Char ' '),
		xPressed = (key == Char 'x')}}
	| keystate == Up   = state0 {input = (input state0) {keySet = Set.delete key (keySet $ input state0)}}
	| otherwise        = state0

motionCallback :: Bool -> GameState -> Position -> GameState
motionCallback _ state0@(GameState (Running pos v (Euler yaw0 pitch0 _)) input' _) (Position x y) =
	state0 { observer = newObserver, input = input' { lastMousePos = Just (x,y) } }
	where
		(x0,y0) = case lastMousePos input' of Nothing -> (x,y)
		                                      Just (x0',y0') -> (x0',y0')
		newObserver = Running pos v (Euler yaw pitch 0)
		dx = 0.002*realToFrac (x - x0)
		dy = 0.002*realToFrac (y - y0)
		yaw = yaw0 + dx
		pitch = bound (-pi/2.1) (pi/2.1) (pitch0 - dy)
		bound min' max' val
			| val < min' = min'
			| val > max' = max'
			| otherwise  = val
    

drawfun :: GameState -> VisObject Double
drawfun (GameState (Running _ _ _) _ (SLAM cams _ ps)) = VisObjects $ 
	[drawBackground, drawCamTrajectory 0.2 cams] 
	-- ++ map drawParticle ps
	++ map drawTrueLandmark trueMap
	-- ++ zipWith drawLandmark [1..] (if null ps then [] else Set.toList $ mergeMapsMAP ps)
	
   
drawBackground :: VisObject Double
drawBackground = VisObjects [Axes (1, 25), Plane (V3 0 1 0) (makeColor 0.5 0.5 0.5 1)]

-- | Takes the seed as an argument.
drawLandmark :: Int -> Landmark -> VisObject Double
drawLandmark seed l = Points (map vec2v3 (take 50 $ samples l seed)) (Just 3) (makeColor 0 0 0 1) where
	vec2v3 v = V3 (v@>0) (v@>1) (v@>2)
	
--drawParticle :: Particle -> VisObject Double
--drawParticle (Particle w cs _) = drawCamTrajectory w cs
	
-- | TODO: Display number
drawTrueLandmark :: (LID, V3 Double) -> VisObject Double
drawTrueLandmark (lid, pos) = Trans pos $ Sphere 0.15 Wireframe (makeColor 0.2 0.3 0.8 1)

-- | Draw a camera with a pre-set weight
drawCamTrajectory :: Double -> [ExactCamera] -> VisObject Double
drawCamTrajectory _ [] = VisObjects []
drawCamTrajectory w (ExactCamera cp cr:cs) = VisObjects $
	Line (v2V cp : map (\(ExactCamera p _) -> v2V p) cs) (makeColor 0 0 1 1) : [drawCam]
		 where
			drawCam = Trans (v2V cp) $ VisObjects 
					[ Cube w Wireframe (makeColor 1 0 0 1)
					, Line [V3 0 0 0, v2V $ cr <> (3|> [0,0,0.2])] (makeColor 1 0 0 1) ]
			v2V v = V3 (v@>0) (v@>1) (v@>2)

	
main :: IO ()
main = do
	let 
		state0 = GameState 
				(Running (V3 (-10) (-7) (-5)) 0 (Euler 1 (-0.6) 0)) 
				(Input (Set.empty) Nothing False False)
				(SLAM [ExactCamera (3|> repeat 0) (ident 3)] [] (replicate 100 ([(undefined,undefined)]) ))
		setCam (GameState x _ _) = setCamera x
		drawfun' x = return (drawfun x, Just None)
	_ <- initThreads
	playIO Nothing "play test" ts state0 drawfun' simfun setCam
		(Just keyMouseCallback) (Just (motionCallback True)) (Just (motionCallback False))
