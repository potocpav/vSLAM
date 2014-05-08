{-# LANGUAGE ForeignFunctionInterface #-}

module RosInterface where

import Numeric.LinearAlgebra ((><), Matrix, trans, toLists)
import System.Environment (getArgs, getProgName)
import Data.List (intercalate)
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import qualified Data.ByteString as B
-- import Foreign.Marshal.Array

import Landmark

{#pointer *keypoint_t as KeypointPtr#}
{#pointer *frame_t as FramePtr#}

--instance Serialize Keypoint where
--	put (Keypoint x y w bs) = put (x,y,w,bs)
--	get = do (x,y,w,bs) <- get; return (Keypoint x y w bs)

get_keypoints :: FramePtr -> IO KeypointPtr
get_keypoints t = {#get frame_t->kps#} t

get_n_kps :: FramePtr -> IO Int
get_n_kps t = {#get frame_t->num_kps#} t >>= return . fromIntegral

get_px, get_py, get_response, get_dt :: KeypointPtr -> IO Double
get_px t = {#get keypoint_t->px#} t >>= return . realToFrac
get_py t = {#get keypoint_t->py#} t >>= return . realToFrac
get_response t = {#get keypoint_t->response#} t >>= return . realToFrac
get_dt t = {#get frame_t->dt#} t >>= return . (\dt -> if dt < 0 then undefined else realToFrac dt)

get_descriptor_size, get_feature_id :: KeypointPtr -> IO Int
get_descriptor_size t = {#get keypoint_t->descriptor_size#} t >>= return . fromIntegral
get_feature_id t = {#get keypoint_t->id#} t >>= return . fromIntegral

get_tf :: FramePtr -> IO (Matrix Double)
get_tf t = do
	values <- sequence [peekByteOff t ({#offsetof frame_t->tf#} + i * {#sizeof tf_t#}) | i <- [0..15]]
	--values <- mapM (\ptr -> {#get tf_t#} ptr >>= return . realToFrac) tf_ptrs
	return $ trans $ (4><4) values

get_descriptor :: Int -> KeypointPtr -> IO B.ByteString
get_descriptor n t = {#get keypoint_t->descriptor#} t >>= (\cchar -> B.packCStringLen (cchar, n))

keypointById :: Int -> KeypointPtr -> KeypointPtr
keypointById i k = plusPtr k ({# sizeof keypoint_t #} * i)

getFrame :: IO (Double, [Feature], Matrix Double)
getFrame = do
	frame_ptr <- extractKeypoints
	kps_ptr <- get_keypoints frame_ptr
	num <- get_n_kps frame_ptr
	
	dt <- get_dt frame_ptr
	kps <- sequence [getKeypoint i kps_ptr | i <- [0..num-1]]
	tf <- get_tf frame_ptr
	return (dt, kps, tf)

getKeypoint i kps_ptr = do
	let kp = keypointById i kps_ptr
	px <- get_px kp
	py <- get_py kp
	response <- get_response kp
	descriptor_size <- get_descriptor_size kp
	descriptor <- get_descriptor descriptor_size kp
	f_id <- get_feature_id kp
	return $ Feature (FID f_id) Nothing (px, py) response descriptor
				
launchRos :: IO ()
launchRos = do
	args <- getArgs
	prog <- getProgName
	_ <- mainC (intercalate ";" (prog:args))
	return ()

peekInt ptr = fmap fromIntegral (peek ptr)

arrDouble :: Matrix Double -> (Ptr CDouble -> IO b) -> IO b
arrDouble matrix foo = let
	array = concat $ toLists $ trans matrix
	carray = map realToFrac array
	in allocaArray (length carray) (\ptr -> pokeArray ptr carray >> foo ptr)

{#fun extract_keypoints as ^
	{ } -> `FramePtr' id#}
 
{#fun publish_tf as ^
	{ arrDouble* `Matrix Double' } -> `()'#}
 
{#fun unsafe main_c as ^
	{ `String' } -> `Int'#}
