{-|
Module      : Playback
License     : WTFPL
Maintainer  : Pavel Potocek <pavelpotocek@gmail.com>

Get the input data from serialized files. Construct a transition function to be
used in the 'updateFilter' routine.
-}
module Playback where

import Text.Printf (printf)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util ((&), zeros)
import System.Directory (doesFileExist)

import Landmark
import Camera
import InternalMath
import Parameters


-- | Acquire a measurement (containing multiple features) from a serialized file.
-- The serialization format is defined by the Prelude show and read functions.
measurement :: Int -> IO (Int, (Double, [Feature], Matrix Double))
measurement i = do
	let filename = printf "../data/yard4-v2/features_%04d.data" i
	exists <- doesFileExist filename
	if exists then do
		serialized <- readFile filename
		return $ (i, read serialized)
	 else measurement (i+1)



-- | Return the next camera position estimate.
-- Note that when this function is partially applied it returns a function
-- ExactCamera -> GaussianCamera, which can be passed to the FastSLAM update
-- routine.
camTransition :: Double 			-- ^ delta time
              -> Matrix Double 		-- ^ 4x4 OpenGL-like transformation matrix from odometry
              -> ExactCamera 		-- ^ Previous camera pose
              -> GaussianCamera		-- ^ Next camera pose estimate
camTransition dt tf cam' = let 
	prevTf = camToTF cam'
	nextTf = prevTf <> tf
	[[ccr, ccp]] = toBlocks [3] [3,1] nextTf
	
	[x',y',z'] = toList $ (head.toColumns) ccp
	[a',b',g'] = toList $ rotmat2euler ccr
	
	posCov = ccr <> (diag $ scale dt $ 3|> kinematicsPosCov) <> trans ccr
	rotCov = diag $ scale dt $ 3|> kinematicsRotCov
	in GaussianCamera (6|> [x',y',z',a',b',g']) (diagBlock [posCov, rotCov])

