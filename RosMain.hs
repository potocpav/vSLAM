{-|
Module      : RosMain
License     : WTFPL
Maintainer  : Pavel Potocek <pavelpotocek@gmail.com>

The entry point of the fastslam_2 package. 

It calls the C++ code (linked as a shared library) to perform ROS initialization 
and hook a callback function to the camera publishing function. After receiving 
a frame, it computes the next FastSLAM 2.0 iteration and returns the results to 
another C++ function that publishes them in ROS.

It is capable to serializing the input into a file usable by the 'Display'
module.
-}

import Numeric.LinearAlgebra
import qualified Data.Set as S
import Text.Printf (printf)
import Text.Show.Pretty
import RosInterface

import Data.Random hiding (sample)
import Data.Random.Source.DevRandom

import Playback (camTransition)
import Landmark
import Camera
import FastSLAM2
import Parameters (numberOfParticles)



main = do
	-- Launch ROS
	launchRos
	-- Launch the main loop
	loop $ replicate numberOfParticles (ExactCamera (3|> [0,0,0]) (ident 3), S.empty)


loop :: [(ExactCamera, Map)] -> IO ()
loop particles = do
	(id, dt,kps,tf) <- getFrame
	
	putStrLn $ "Processing the acquired frame no. " ++ show id
	
	particles' <- (flip runRVar) DevURandom (filterUpdate 
		 particles
		 (camTransition dt tf)
		 kps)
		
	let cam = averageCams (map fst particles)
	publishTf (camToTF cam)
	
	-- saveData (id,dt,kps,tf)
	if id > 0 then print dt else return ()
	loop particles'

-- | Serialize the data acquired from ROS in a 'Display' module compatible format.
-- The function is not a simple prettyPrint, but the difference is only in whitespace.
-- This is because the prettyPrint function put the whole output on a single line.
-- And we gotta change that! :-)
saveData :: (Int, Double, [Feature], Matrix Double) -> IO ()
saveData (id, dt, kps, tf) = let
		prettyPrintTuple :: Double -> [Feature] -> Matrix Double -> String
		prettyPrintTuple dt kps tf = "( " ++ ppShow dt ++ "\n, " ++ ppShow kps ++ "\n, " ++ ppShow tf ++ "\n)"
	in do 
	putStrLn "Saving the data..."
	writeFile (printf "/home/pavel/Documents/test/features_%04d.data" id) (prettyPrintTuple dt kps tf)
