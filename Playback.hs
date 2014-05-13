-- Get the input data from Serialized files.

module Playback where

import Text.Printf (printf)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util ((&), zeros)
import System.Directory (doesFileExist)

import Landmark
import Camera
import InternalMath
import Parameters

measurement :: Int -> IO (Int, (Double, [Feature], Matrix Double))
measurement i = do
	let filename = printf "../data/yard4-v2/features_%04d.data" i
	exists <- doesFileExist filename
	if exists then do
		serialized <- readFile filename
		return $ (i, read serialized)
	else measurement (i+1)



-- | Return the next camera position estimate.
-- Note, that this function is typically partially applied, returning
-- ExactCamera -> GaussianCamera, passed to the FastSLAM routine.
camTransition :: Double 			-- ^ delta-time
              -> Matrix Double 		-- ^ 4x4 transformation matrix from odometry
              -> ExactCamera 		-- ^ Previous camera pose
              -> GaussianCamera		-- ^ Next camera position estimate
camTransition dt tf cam' = let 
	prevTf = camToTF cam'
	nextTf = prevTf -- <> tf
	[[ccr, ccp]] = toBlocks [3] [3,1] nextTf
	
	[x',y',z'] = toList $ (head.toColumns) ccp
	[a',b',g'] = toList $ rotmat2euler ccr
	
	posCov = ccr <> (diag $ scale dt $ 3|> kinematicsPosCov) <> trans ccr
	rotCov = diag $ scale dt $ 3|> kinematicsRotCov
	in GaussianCamera (6|> [x',y',z',a',b',g']) (diagBlock [posCov, rotCov])

-- | Compute the velocity from the most recent camera estimates.
camVelocity :: [ExactCamera] -> Vector Double
camVelocity [ExactCamera cp1 _, ExactCamera cp2 _] =
	(cp1 - cp2) & (3|> repeat 0)
camVelocity _ = 6|> repeat 0
