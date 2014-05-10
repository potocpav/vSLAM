
import Numeric.LinearAlgebra
-- import Control.Concurrent (threadDelay)
-- import qualified Data.ByteString as BS
import Text.Printf (printf)
import Text.Show.Pretty
import RosInterface

import Data.Random hiding (sample)
import Data.Random.Source.DevRandom

import Landmark
import Camera
import FastSLAM2

main = do
	-- Launch ROS
	launchRos
	-- Launch the main loop
	loop undefined
{-
[(ExactCamera, Map)] 
             -> (ExactCamera -> GaussianCamera) 
             -> [Feature] -}
loop :: [(ExactCamera, Map)] -> IO ()
loop particles' = do
	(id, dt,kps,tf) <- getFrame
	
	particles <- (flip runRVar) DevURandom $ filterUpdate 
		$ particles'
		$ undefined
		$ undefined
		
		
	publishTf tf
	
	if id > 0 then print dt else return ()
	loop undefined

saveData :: (Int, Double, [Feature], Matrix Double) -> IO ()
saveData (id, dt, kps, tf) = let
		prettyPrintTuple :: Double -> [Feature] -> Matrix Double -> String
		prettyPrintTuple dt kps tf = "( " ++ ppShow dt ++ "\n, " ++ ppShow kps ++ "\n, " ++ ppShow tf ++ "\n)"
	in do 
	putStrLn "Saving the data..."
	writeFile (printf "/home/pavel/Documents/test/features_%04d.data" id) (prettyPrintTuple dt kps tf)
