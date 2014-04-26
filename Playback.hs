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
	bs <- BS.readFile (printf "../data/squares/features_%04d.data" i)
	return $ case decode bs of 
		Left err -> []
		Right val -> map (\v -> v { fpos = (\(theta, phi) -> (deformt theta, deformp (-phi))) $ fpos v }) val where
			deformt theta = theta -- debug "theta" ((theta + pi)*0.999-pi) -- if theta < 0 then theta else theta + 1/500*2*pi
			deformp phi = phi -- debug "phi" phi

camTransition :: ExactCamera -> GaussianCamera
camTransition (ExactCamera ccp ccr) = 
	GaussianCamera (6|> [x',y',z',a',0,0]) ((posCov ! empty33) # (empty33 ! (diag $ 3|> [0.3,0.03,0.03])))
	where 
		[x',y',z'] = toList ccp
		[a',b',g'] = toList $ rotmat2euler ccr
		posCov = ccr <> (diag $ 3|> [0.01,0.01, 0.01]) <> trans ccr
		empty33 = zeros 3 3
