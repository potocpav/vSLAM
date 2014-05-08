
import Numeric.LinearAlgebra
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as BS
import Text.Printf (printf)
import Text.Show.Pretty
import RosInterface
import Landmark

main = do
	-- Launch ROS
	launchRos
	-- Launch the main loop
	loop 1

loop :: Int -> IO ()
loop i = do
	putStrLn $ "Extracting point in frame " ++ show i ++ "."
	(dt,kps,tf) <- getFrame
	
	putStrLn "Saving the data..."
	writeFile (printf "/home/pavel/Documents/test/features_%04d.data" i) $ prettyPrintTuple dt kps tf
	
	if i > 0 then print dt else return ()
	loop (i+1)

prettyPrintTuple :: Double -> [Feature] -> Matrix Double -> String
prettyPrintTuple dt kps tf = "( " ++ ppShow dt ++ "\n, " ++ ppShow kps ++ "\n, " ++ ppShow tf ++ "\n)"
