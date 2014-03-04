{-# OPTIONS_GHC -Wall #-}

module Main ( main
            ) where

import Linear
import SpatialMath ( Euler(..), rotateXyzAboutY, rotVecByEulerB2A, rotateXyzAboutX )
import Graphics.X11 ( initThreads )
import Vis
import Graphics.UI.GLUT hiding ( Plane, Sphere, Points, motionCallback, samples )
import qualified Data.Set as Set

import Control.Monad ( when )

import Feature
import Numeric.LinearAlgebra

ts :: Double
ts = 0.01

faceHeight :: Double
faceHeight = 1.5

data PlayerState = Running (V3 Double) (V3 Double) (Euler Double)

data GameState = GameState { playerState :: PlayerState
                           , keySet :: Set.Set Key
                           , lastMousePos :: Maybe (GLint,GLint)
                           }

toVertex :: (Real a, Fractional b) => V3 a -> Vertex3 b
toVertex xyz = (\(V3 x y z) -> Vertex3 x y z) $ fmap realToFrac xyz

setCamera :: PlayerState -> IO ()
setCamera (Running (V3 x y z) _ euler) = lookAt (toVertex xyz0) (toVertex target) (Vector3 0 (-1) 0)
  where
    xyz0 = V3 x (y-faceHeight) z
    target = xyz0 + rotateXyzAboutY (rotateXyzAboutX (rotVecByEulerB2A euler (V3 1 0 0)) (-pi/2)) (-pi/2)

simfun :: Float -> GameState -> IO GameState
simfun _ (GameState (Running pos _ euler0@(Euler yaw _ _)) keys lmp) = do
  Size x y <- get windowSize
  let x' = (fromIntegral x) `div` 2
      y' = (fromIntegral y) `div` 2

  when (Just (x',y') /= lmp) (pointerPosition $= (Position x' y'))
  return $ GameState (Running (pos + (ts *^ v)) v euler0) keys (Just (x',y'))
  where
    v = rotateXyzAboutY (V3 (d-a) 0 (w-s)) yaw
      where
        w = if Set.member (Char 'w') keys then 3 else 0
        a = if Set.member (Char 'a') keys then 3 else 0
        s = if Set.member (Char 'r') keys then 3 else 0
        d = if Set.member (Char 's') keys then 3 else 0

keyMouseCallback :: GameState -> Key -> KeyState -> Modifiers -> Position -> GameState
keyMouseCallback state0 key keystate _ _
  | keystate == Down = state0 {keySet = Set.insert key (keySet state0)}
  | keystate == Up   = state0 {keySet = Set.delete key (keySet state0)}
  | otherwise        = state0

motionCallback :: Bool -> GameState -> Position -> GameState
motionCallback _ state0@(GameState (Running pos v (Euler yaw0 pitch0 _)) _ lmp) (Position x y) =
  state0 {playerState = newPlayerState, lastMousePos = Just (x,y)}
  where
    (x0,y0) = case lmp of Nothing -> (x,y)
                          Just (x0',y0') -> (x0',y0')
    newPlayerState = Running pos v (Euler yaw pitch 0)
    dx = 0.002*realToFrac (x - x0)
    dy = 0.002*realToFrac (y - y0)
    yaw = yaw0 + dx
    pitch = bound (-pi/2.1) (pi/2.1) (pitch0 - dy)
    bound min' max' val
      | val < min' = min'
      | val > max' = max'
      | otherwise  = val
    

drawfun :: GameState -> VisObject Double
drawfun (GameState (Running _ _ _) _ _) =
  VisObjects $ [axes,box,ellipsoid,sphere, drawFeature feature1, drawFeature feature2, drawFeature feature3, plane,boxText] 
  where
    x' = -1
    axes = Axes (1, 25)
    sphere = Trans (V3 0 x' (-1)) $ Sphere 0.15 Wireframe (makeColor 0.2 0.3 0.8 1)
    ellipsoid = Trans (V3 x' 0 (-1)) $ Ellipsoid (0.2, 0.3, 0.4) Wireframe (makeColor 1 0.3 0.5 1)
    box = Trans (V3 0 0 x') $ Box (0.2, 0.2, 0.2) Wireframe (makeColor 0 1 1 1)
    plane = Plane (V3 0 1 0) (makeColor 1 0 0 1)
    -- text k = Text2d "OLOLOLOLOLO" (100,500 - k*100*x') TimesRoman24 (makeColor 0 (0.5 + x''/2) (0.5 - x''/2) 1)
    --  where
    --    x'' = realToFrac $ (x' + 1)/0.4*k/5
    boxText = Text3d "trololololo" (V3 0 0 (x'-0.2)) TimesRoman24 (makeColor 1 0 0 1)
    feature1 = Feature 1 (6|> [0,0,0,0,0,0.2]) (diag (6|> [0,0,0,0.01,0.01,0.02]))
    feature2 = Feature 1 (6|> [3,0,0,-0.5,0,0.5]) (diag (6|> [0,0,0,0.01,0.01,0.5]))
    feature3 = Feature 1 (6|> [0,-5,3,2,-1,0.5]) (diag (6|> [0,0,0,0.01,0.01,0.5]))
    
drawFeature :: Feature -> VisObject Double
drawFeature f = Points (map vec2v3 (take 300 $ samples f 10)) (Just 3) (makeColor 0 0 0 1) where
	vec2v3 v = V3 (v@>0) (v@>1) (v@>2)
	
	
main :: IO ()
main = do
  let state0 = GameState (Running (V3 0 0 0) 0 (Euler 0 0 0)) (Set.empty) Nothing
      setCam (GameState x _ _) = setCamera x
      drawfun' x = return (drawfun x, Just None)
  _ <- initThreads
  playIO Nothing "play test" ts state0 drawfun' simfun setCam
    (Just keyMouseCallback) (Just (motionCallback True)) (Just (motionCallback False))
