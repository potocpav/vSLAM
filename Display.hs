{-# LANGUAGE PatternGuards #-}

-- | This module uses Gloss 2D library to visualize the SLAM filter.
module Display (display) where

import Feature
--import EKF2D
import Simulate
import PHDSLAM

import qualified Data.Matrix as M
import qualified Data.Vector as V
--import qualified Data.Set as S

import Graphics.Gloss hiding (Vector,Point)
import Graphics.Gloss.Interface.IO.Game hiding (Vector,Point)
import Data.Random hiding (sample)

import Data.Random.Source.DevRandom

-- for debugging
import System.IO.Unsafe (unsafePerformIO)

debug :: Show a => a -> a
debug a = unsafePerformIO (print a) `seq` a


data View = View {vx :: Float, vy :: Float, zoom :: Float}

-- | TODO: Kill camera, because it is already in particles
data World = World { landmarks :: [Point], particles :: [Particle], camera :: Camera }

data State = State 
		{ view :: View
		, world :: World
		, viewMousePos :: (Maybe Point) -- last position of mouse when moving the camera
		, leftButton :: (Maybe Point) 
		}

(width, height) = (600, 600) :: (Float,Float)
landmark = Color blue $ Circle 0.05

-- | Display a feature. The first parameter is a feature intensity, from an interval (0,1).
dispFeature :: Feature -> Picture
dispFeature f@(Feature eta mu cov) = pictures $ line : shownPoints where
	line = Color red $ Line [(mu!0, mu!1), (mu!0 + sin(mu!2)/ (mu!3), mu!1 + cos(mu!2) / (mu!3))]
	points = samples f seed
	seed = floor $ (eta + M.trace cov + V.sum mu) * 10^10
	shownPoints = (\v -> Translate (v!0) (v!1) . Color (if mu!3 > 0 then black else red) 
				$ pictures [Line [(-0.05,0),(0.05,0)], Line [(0,-0.05),(0,0.05)]]) 
				`fmap` take (floor (1000*eta)) points -- this number is nr. of points
	(!) = (V.!)
	

main = do
	initial_landmarks <- initial
	print $ measurement (Camera (0, 0) 0) initial_landmarks

	let initial = State 
		(View 0 0 20) 
		(World 
			initial_landmarks
			[(1,Camera (0,0) 0, [])]
			(Camera (0, 0) 0)
		)
		Nothing
		Nothing
	playIO	(InWindow "Draw" (floor width, floor height) (0,0))
			white 100 initial
			makePicture handleEvent stepWorld


dispBackground :: Picture
dispBackground = Color (greyN 0.7) $ pictures
	[ Circle 1
	, Line [(-10,0),(10,0)]
	, Line [(0,-10),(0,10)]
	]

dispLandmark :: Point -> Picture
dispLandmark a = uncurry Translate a landmark

dispCamera :: Camera -> Picture
dispCamera (Camera (x, y) phi) = Color (dark green) $ pictures
	[ Line [(x,y), posPlusPhi (phi+pi/4), posPlusPhi (phi-pi/4), (x,y)]
	, Translate x y $ Circle 0.2 ] where
	posPlusPhi phi = let scale=sqrt 2 in 
		(x+sin(phi)*scale,y+cos(phi)*scale)


-- | Convert our state to a picture.
makePicture :: State -> IO Picture
makePicture (State view world _ _) = return $ viewTransform picture where
	viewTransform = Scale scale scale . Translate (-vx view) (-vy view)
	scale = min width height / zoom view
	
	picture = pictures $ [dispBackground] 
		++ map dispLandmark (landmarks world)
		++ [dispCamera (camera world)]
		++ map dispFeature  (concatMap (\(_,_,f) -> f) (particles world))
		
{-	do
	let scale = min width height / zoom view
	let viewTransform = Scale scale scale . Translate (-vx view) (-vy view)
	
	let purePics = [dispBackground] 
		++ map dispLandmark (landmarks world)
		++ [dispCamera (camera world)]
	ioPics <- mapM dispFeature (concatMap (\(_,_,f) -> f) (particles world))
	return $ viewTransform (pictures (purePics ++ ioPics))
-}
handleEvent :: Event -> State -> IO State
handleEvent event state

	-- mouse control of the camera

	| EventKey (MouseButton LeftButton) Down _ pt <- event
	= return $ state { world = (world state) { 
					camera = Camera (toWorld (view state) pt) 0 }   
		    , leftButton = Just pt
	        }
	
	| EventKey (MouseButton LeftButton) Up _ _ <- event
	= do
		let cam = camera (world state)
		p <- (flip runRVar) DevURandom $ updateParticles 
					(measurement cam (landmarks (world state)))
					(particles (world state))
					(const $ return cam)
		return $ state
			{ world = (world state) { particles = p }
			, leftButton = Nothing
			}
	
	| EventMotion (x,y) <- event
	, State _ world _ (Just pt@(cx,cy)) <- state
	= let cam = Camera (toWorld (view state) pt) (atan2 (x-cx) (y-cy)) in
		return $  state { world = world { camera = cam } }
	
	-- Mouse control of the view

	| EventMotion (x, y) <- event
	, State (View  vx vy zoom) _ (Just (px, py)) _ <- state
	= let 
		dx = px-x; dy = py-y
		scale = min width height / zoom in return $ 
			state { view         = (View (vx+dx/scale) (vy+dy/scale) zoom)
			      , viewMousePos = Just (x,y)
			}

	| EventKey (MouseButton RightButton) dir _ pt <- event
	= return $ state {viewMousePos = if dir == Down then Just pt else Nothing}

	| EventKey (MouseButton wheel) _ _ _ <- event
	= return $ state {view = (view state) {zoom = zoom (view state) * case wheel of 
		WheelUp -> 0.8
		WheelDown -> 1/0.8
		otherwise -> 1
	}}
	
	-- keyboard controls
	
	| EventKey (Char 'x') Down _ _ <- event
	= return state

	| otherwise
	= return state

toWorld :: View -> Point -> Point
toWorld (View x y z) (sx,sy) = ((sx/width) * z + x, (sy/height) * z + y)

-- | Not varying with time
stepWorld :: Float -> State -> IO State
stepWorld _ a = return a
