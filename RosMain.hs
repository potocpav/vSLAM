
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

main = do
	-- Launch ROS
	launchRos
	-- Launch the main loop
	loop $ replicate 20 (ExactCamera (3|> [0,0,0]) (ident 3), S.empty)


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
	
	if id > 0 then print dt else return ()
	loop particles'

saveData :: (Int, Double, [Feature], Matrix Double) -> IO ()
saveData (id, dt, kps, tf) = let
		prettyPrintTuple :: Double -> [Feature] -> Matrix Double -> String
		prettyPrintTuple dt kps tf = "( " ++ ppShow dt ++ "\n, " ++ ppShow kps ++ "\n, " ++ ppShow tf ++ "\n)"
	in do 
	putStrLn "Saving the data..."
	writeFile (printf "/home/pavel/Documents/test/features_%04d.data" id) (prettyPrintTuple dt kps tf)
