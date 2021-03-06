{-# OPTIONS_GHC -Wall #-}
{-|
Module      : Display
License     : WTFPL
Maintainer  : Pavel Potocek <pavelpotocek@gmail.com>

This module provides a 3D simulation environment for the FastSLAM2 routine.

It simulates over the data supplied by the 'Playback' module. It has got a 
'main' function and is standalone (separate from the 'RosMain' module).
-}

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
import Data.List (sort)
import Numeric.LinearAlgebra

import Landmark
import Camera
import Playback --import Simulate
import FastSLAM2

import System.CPUTime
import Text.Printf

ts :: Double
ts = 0.01

data ObserverState = Running (V3 Double) (V3 Double) (Euler Double)
data InputState = Input { keySet :: Set.Set Key, lastMousePos :: Maybe (GLint, GLint), spacePressed :: Bool, xPressed :: Bool }

-- | The complete state updated by the FastSLAM2 routine
data SLAMState = SLAM 
	{ frameId :: Int
	, camHistory :: [ExactCamera]       -- ^Averaged camera history
	, camHistories :: [[ExactCamera]]   -- ^Per-particle camera histories
	, particles :: [(ExactCamera, Map)]
	}

-- | The complete simulation state.
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


-- | The main state update routine.
simfun :: Float -> GameState -> IO GameState
simfun _ (GameState (Running pos _ euler0@(Euler yaw _ _)) input' (SLAM last_frame_id chist chists ps)) = do
	Size x y <- get windowSize
	let 
		x' = (fromIntegral x) `div` 2
		y' = (fromIntegral y) `div` 2
		run = id $ spacePressed input'
		
	(frame_id, (dt,meas,tf)) <- if run 
			then measurement (last_frame_id+1) 
			else return (last_frame_id, (undefined, [], undefined))
	if run then putStrLn $ "dt for frame " ++ show frame_id ++ ": " ++ show dt else return ()
	 
	-- | Run the FastSLAM routine.
	ps' <- if not run then return ps else
		(flip runRVar) DevURandom $ filterUpdate
				ps
				(camTransition dt tf)
				meas
	
	if run then printBestLandmarks (snd $ head ps') frame_id else return ()
	
	let chists' = if run then (map fst ps') : chists else chists
	let chist' = if run then (averageCams $ map fst ps') : chist else chist
	
	when (Just (x',y') /= lastMousePos input') (pointerPosition $= (Position x' y'))

	return $ GameState 
		(Running (pos + (ts *^ v)) v euler0) 
		input' { lastMousePos = Just (x',y'), spacePressed = False, xPressed = False }
		(SLAM frame_id chist' chists' ps') where
			keyPressed k = Set.member (Char k) (keySet input')
			v = rotateXyzAboutY (V3 (d-a) (dn-up) (w-s)) yaw where
					w = if keyPressed 'w' then 10 else 0
					a = if keyPressed 'a' then 10 else 0
					s = if keyPressed 'r' then 10 else 0
					d = if keyPressed 's' then 10 else 0
					up = if keyPressed 'p' then 10 else 0
					dn = if keyPressed 't' then 10 else 0


printBestLandmarks :: Map -> Int -> IO ()
printBestLandmarks m frame_id = 
	putStrLn . show . map get_age . take 20 . sort $ Set.toList m
	where get_age lm = frame_id - (floor $ fromIntegral (fromLID $ lid lm) / (1000 :: Double))
	

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
    

-- | The main drawing routine. It converts a 'GameState' into a 'VisObject',
-- which is displayed on the screen by the not-gloss package.
drawfun :: GameState -> VisObject Double
drawfun (GameState (Running camPos _ _) _ (SLAM frame_id chist chists ps)) = VisObjects $ 
	[drawMap . snd $ head ps]
	++ [drawCamTrajectory 0.1 chist]
	++ (if length chists > 0 then map (drawCamTrajectory 0.05 . return) (head chists) else [])
	++ [drawBackground camPos]
	++ [Text2d ("Frame "++show frame_id) (10,10) Helvetica10 (makeColor 0 0 0 1)]
	-- ++ map drawTrueLandmark trueMap
	-- ++ zipWith drawLandmark [1..] (if null ps then [] else Set.toList $ mergeMapsMAP ps)
	

-- | Draw the grid and the axes to give a visual clue of positions and depths.
-- The grid automatically moves with the camera steps.
drawBackground :: V3 Double -> VisObject Double
drawBackground (V3 x _ z) = VisObjects [Axes (1, 25), Trans (V3 x' 0 z') $ Plane (V3 0 1 0) (makeColor 0.5 0.5 0.5 1) (makeColor 0 0 0 0)]
	where 
	(x',z') = (roundToTwos x, roundToTwos z)
	roundToTwos v = 2*(fromIntegral $ (round (v/2) :: Int))


-- | Draw a landmark as a particle set. The first argument is a seed. That way
-- this function does not have to be in the IO monad.
drawLandmark :: Int -> Landmark -> VisObject Double
drawLandmark seed l = Points (map vec2v3 (take 10 $ samples l seed)) (Just 3) (makeColor 1 1 1 1) where
	vec2v3 v = V3 (v@>0) (v@>1) (v@>2)


-- | Draw a whole bunch of landmarks. Draw only those with sufficient health
-- to avoid clutter.
drawMap :: Map -> VisObject Double
drawMap m = VisObjects $ map (drawLandmark 1) (filter (\l -> lhealth l > 1.5) $ Set.toList m)


-- | Draw a camera along with its trajectory with a pre-set size of the 
-- camera mesh.
drawCamTrajectory :: Double -> [ExactCamera] -> VisObject Double
drawCamTrajectory _ [] = VisObjects []
drawCamTrajectory w (ExactCamera cp cr:cs) = VisObjects $
	Line (v2V cp : map (\(ExactCamera p _) -> v2V p) cs) (makeColor 0 1 0 1) : [drawCam]
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
				(SLAM 50 [] [] (replicate 20 (ExactCamera (3|> [0,0,0]) (ident 3), Set.empty) ))
		setCam (GameState x _ _) = setCamera x
		drawfun' x = return (drawfun x, Just None)
	_ <- initThreads
	playIO Nothing "play test" ts state0 drawfun' simfun setCam
		(Just keyMouseCallback) (Just (motionCallback True)) (Just (motionCallback False))
