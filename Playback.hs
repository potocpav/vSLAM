-- Get the input data from Serialized files.

module Playback where

import Text.Printf (printf)
import qualified Data.ByteString as BS
import Data.Serialize
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util ((&), (!), (#), zeros)

import Landmark
import Camera
import InternalMath

measurement :: Int -> IO [Feature]
measurement i = do
	bs <- BS.readFile (printf "../data/yard4/features_%04d.data" i)
	return $ case decode bs of 
		Left err -> []
		Right val -> map (\v -> v { fpos = (\(theta, phi) -> (theta, -phi)) $ fpos v }) val

camTransition :: ExactCamera -> GaussianCamera
camTransition (ExactCamera ccp ccr) = 
	GaussianCamera (ccp & rotmat2euler ccr) ((posCov ! empty33) # (empty33 ! (diag $ 3|> [0.03,0.03,0.03])))
	where 
		posCov =  ccr <> (diag $ 3|> [0.02, 0.000003, 0.1]) <> trans ccr
		empty33 = zeros 3 3
