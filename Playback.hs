-- Get the input data from Serialized files.

module Playback where

import Text.Printf (printf)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util ((&), (!), (#), zeros)

import Landmark
import Camera
import InternalMath

measurement :: Int -> IO (Double, [Feature], Matrix Double)
measurement i = do
	serialized <- readFile (printf "../data/yard4-v2/features_%04d.data" i)
	return $ read serialized


-- | Return the next camera position estimate.
-- Note, that this function is typically partially applied, returning
-- ExactCamera -> GaussianCamera, passed to the FastSLAM routine.
camTransition :: Double 			-- ^ delta-time
              -> Matrix Double 		-- ^ 4x4 transformation matrix from odometry
              -> ExactCamera 		-- ^ Previous camera pose
              -> GaussianCamera		-- ^ Next camera position estimate
camTransition dt tf (ExactCamera ccp' ccr') = let 
	prevTf = (ccr' ! asColumn ccp') # (1><4) [0,0,0,1]
	nextTf = prevTf <> tf
	[[ccr, ccp]] = toBlocks [3] [3,1] nextTf
	
	[x',y',z'] = toList $ (head.toColumns) ccp
	[a',b',g'] = toList $ rotmat2euler ccr
	
	posCov = ccr <> (diag $ scale dt $ 3|> [0.05, 0.05, 0.1]) <> trans ccr
	rotCov = diag $ scale dt $ 3|> [0.01, 0.01, 0.01]
	in GaussianCamera (6|> [x',y',z',a',b',g']) (diagBlock [posCov, rotCov])

-- | Compute the velocity from the most recent camera estimates.
camVelocity :: [ExactCamera] -> Vector Double
camVelocity [ExactCamera cp1 _, ExactCamera cp2 _] =
	(cp1 - cp2) & (3|> [0..])
camVelocity _ = 6|> repeat 0
