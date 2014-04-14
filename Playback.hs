-- Get the input data from Serialized files.

module Playback where

import Text.Printf (printf)
import qualified Data.ByteString as BS
import Data.Serialize
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util ((&))

import Landmark
import Camera
import InternalMath

measurement :: Int -> IO [Feature]
measurement i = do
	bs <- BS.readFile (printf "../data/yard1/features_%04d.data" i)
	return $ case decode bs of 
		Left err -> []
		Right val -> map (\v -> v { fpos = (\(theta, phi) -> (theta, -phi)) $ fpos v }) val

camTransition :: ExactCamera -> GaussianCamera
camTransition (ExactCamera ccp ccr) = let 
	in GaussianCamera (ccp & rotmat2euler ccr) (diag (6|> [0.1,0.005,0.1,0.03,0.01,0.01]))
