
module Display (display) where

import Feature
import EKF2D
import Simulate

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Set as S

import Graphics.Gloss hiding (Vector,Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector,Point)
import Data.Random.Normal

-- for debugging
import System.IO.Unsafe (unsafePerformIO)

debug :: Show a => a -> a
debug a = unsafePerformIO (print a) `seq` a


data View = View {vx :: Float, vy :: Float, zoom :: Float}

data World = World { landmarks :: [Point], features :: [Feature], camera :: Camera }

data State = State 
		{ view :: View
		, world :: World
		, viewMousePos :: (Maybe Point) -- last position of mouse when moving the camera
		, leftButton :: (Maybe Point) 
		}

(width, height) = (600, 600) :: (Float,Float)
landmark = Color blue $ Circle 0.05

-- | Return a random point from the feature distribution, and converted to
-- euclidean space
sample :: Feature -> Int -> V.Vector Float
sample (Feature mu cov) seed = toXY random4 where
	random4 = M.getCol 1 $ M.colVector mu + cov * randomStd seed
	randomStd seed = M.colVector $ V.fromList (take 4 $ mkNormals' (0,1) seed)

-- | Return a pseudo-random infinite list of points. List made of similar seeds are
-- themselves similar.
samples :: Feature -> Int -> [V.Vector Float]
samples feature seed = map (sample feature) [seed..]

dispFeature :: Feature -> Picture
dispFeature f@(Feature mu cov) = pictures $ lines ++ shownPoints where
	lines = [Color red $ Line [(mu!0, mu!1), (mu!0 + sin(mu!2)/ (mu!3), mu!1 + cos(mu!2) / (mu!3))]]
	points = samples f 8000 -- this number is seed
	shownPoints = (\v -> Translate (v!0) (v!1) . Color (if mu!3 > 0 then black else red) 
				$ Line [(-0.1,0),(0.1,0)]) 
				`fmap` take 100 points -- this number is nr. of points
	(!) = (V.!)

main = do
	initial_landmarks <- initial
	print $ measurement (Camera (0, 0) 0) initial_landmarks
	
	let initial = State 
		(View 0 0 20) 
		(World 
			initial_landmarks
			[]
			(Camera (0, 0) 0)
		)
		Nothing
		Nothing
	play	(InWindow "Draw" (floor width, floor height) (0,0))
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
makePicture :: State -> Picture
makePicture (State view world _ _) = viewTransform picture where
	viewTransform = Scale scale scale . Translate (-vx view) (-vy view)
	scale = min width height / zoom view
	
	picture = pictures $ [dispBackground] 
		++ map dispLandmark (landmarks world)
		++ [dispCamera (camera world)]
		++ map dispFeature (features world)
	

handleEvent :: Event -> State -> State
handleEvent event state

	-- mouse control of the camera

	| EventKey (MouseButton LeftButton) Down _ pt <- event
	= state { world = (world state) { 
					camera = Camera (toWorld (view state) pt) 0 }   
		    , leftButton = Just pt
	        }
	
	| EventKey (MouseButton LeftButton) Up _ _ <- event
	= state { leftButton = Nothing }
	
	| EventMotion (x,y) <- event
	, State _ world _ (Just pt@(cx,cy)) <- state
	= let cam = Camera (toWorld (view state) pt) (atan2 (x-cx) (y-cy)) in
	state { world = world 
				{ camera = cam 
				, features = map (\a -> initialize cam (a,0.1)) $ measurement cam (landmarks world) }
	}
	
	-- Mouse control of the view

	| EventMotion (x, y) <- event
	, State (View  vx vy zoom) _ (Just (px, py)) _ <- state
	= let 
		dx = px-x; dy = py-y
		scale = min width height / zoom in
			state { view         = (View (vx+dx/scale) (vy+dy/scale) zoom)
			      , viewMousePos = Just (x,y)
			}

	| EventKey (MouseButton MiddleButton) dir _ pt <- event
	= state {viewMousePos = if dir == Down then Just pt else Nothing}

	| EventKey (MouseButton wheel) _ _ _ <- event
	= state {view = (view state) {zoom = zoom (view state) * case wheel of 
		WheelUp -> 0.8
		WheelDown -> 1/0.8
		otherwise -> 1
	}}
	
	-- keyboard controls
	
	| EventKey (Char 'x') Down _ _ <- event
	= state

	| otherwise
	= state

toWorld :: View -> Point -> Point
toWorld (View x y z) (sx,sy) = ((sx/width) * z + x, (sy/height) * z + y)

-- | Not varying with time
stepWorld :: Float -> State -> State
stepWorld _ = id
