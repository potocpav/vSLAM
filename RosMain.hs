
import Control.Concurrent (threadDelay)
import Data.Serialize
import qualified Data.ByteString as BS
import Text.Printf (printf)

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
	(dt,kps) <- getFrame
	
	putStrLn "Saving the data..."
	BS.writeFile (printf "/home/pavel/Documents/test/features_%04d.data" i) (encode kps) 
	
	--sequence . map (putStrLn . show) $ kps
	if i > 0 then print dt else return ()
	--BS.appendFile "../kps.dump" (encode kps)
	loop (i+1)
