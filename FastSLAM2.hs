{-# OPTIONS -Wall #-}
{-|
Module      : FastSLAM2
License     : WTFPL
Maintainer  : Pavel Potocek <pavelpotocek@gmail.com>

This package is an implementation of the FastSLAM 2.0 routine.

For it to work properly, the camera needs not to be stationary for a prolonged
period of time. Consider implementing the code for the detection of a stationary
camera and drop any excess frames.
-}

module FastSLAM2 where

import Numeric.LinearAlgebra hiding (find, i)
import Data.Random (RVar)
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Categorical (weightedCategorical)
import Data.Random.Extras (shuffle)
import Data.Maybe (fromJust)
import qualified Data.Set as S

import Landmark
import Camera
import Measurement
import InternalMath
import Parameters


-- | Return a 2D gaussian of the feature search region in a projective space 
-- (azimuth, elevation)
searchRegion :: (GaussianCamera, Landmark) -> Gauss
searchRegion (gcam, lm) = Gauss mu' cov' where
	ecam = gauss2exact gcam
	mu' = (\(t,p) -> 2|> [t,p]) $ measure ecam (lmu lm)
	
	jc = jacobian_c gcam (lmu lm)
	jl = jacobian_l ecam (lmu lm)
	cov' = jc <> (ccov gcam) <> trans jc + jl <> (lcov lm) <> trans jl + measurement_cov
	
	
-- | Delete any landmarks with insufficient health
pruneLandmarks :: Map -> Map
pruneLandmarks ms = S.map fade $ S.filter (\l -> lhealth l > 0) ms where
	fade lm = lm { lhealth = lhealth lm - 1 }
	

-- | Perform the guided matching routine and the camera EKF update. These two
-- are interleaved. Every landmark is matched with the feature set. If the match 
-- is successful, the association is saved into the 'Feature' structure and the
-- camera is EKF updated. The routine returns the updated set and camera.
-- It works recursively until there are no more Landmarks to be processed.
updateCamera :: Double -- min_health
             -> [Landmark] 
             -> (Double, S.Set Feature, GaussianCamera) 
             -> (Double, S.Set Feature, GaussianCamera)
updateCamera _ [] x = x
updateCamera minHealth (lm':ss) (w', fs', gc') =
	case guidedMatch (lm', searchRegion (gc', lm')) fs' of
	Nothing -> updateCamera minHealth ss (w'*0.2, fs', gc')
	Just (w,f) -> let
		gc = singleFeatureCameraUpdate gc' f
		fs = S.insert f fs'						-- Replace an old feature with an associated one.
		in if lhealth (fromJust (flm f)) >= minHealth 
			then updateCamera minHealth ss (w*w', fs, gc) 
			else updateCamera minHealth ss (w', fs, gc')
	

-- | Check the search region for a corresponding feature. If found, return
-- the update weight and an updated 'Feature' with the association saved.
guidedMatch :: (Landmark, Gauss) -> S.Set Feature -> Maybe (Double, Feature)
guidedMatch (lm, g) fs = let
	f_pos f = (\(a,b) -> 2|> [a,b]) $ fpos f
	
	neighbors :: S.Set Feature
	neighbors =  {- unsafePerformIO (putStrLn $ show (fromLID $ lid lm) ++ show (map dist $ S.toList ff)) `seq` -} 
		mahalFilter . hammingFilter $ fs where
		mahalFilter = S.filter (\f -> mahalDist_sq g (f_pos f) < 3*3)
		hammingFilter = S.filter (\f -> dist f < 40)
		

	-- | If just one feature matches the criteria, associate it
	updated :: Maybe Feature
	updated = if S.size neighbors == 1 then Just $ (head $ S.elems neighbors) {flm = Just lm} else Nothing
	
	dist f' = hammingDist (ldescriptor lm) (descriptor f')
	w f = normalDensity g ((\(a,b) -> 2|> [a,b]) (fpos f))
	in case updated of
		Nothing -> Nothing 
		Just up -> Just (w up, up)


cameraSample :: GaussianCamera -> RVar ExactCamera
cameraSample (GaussianCamera mu cov) = do
	rndVec <- sequence (replicate 6 stdNormal)
	let [cx,cy,cz,a,b,g] = toList $ mu + cov <> (6 |> rndVec)
	return $ ExactCamera (3|> [cx,cy,cz]) (euler2rotmat (3|> [a,b,g]))


-- | Perform a standard resampling routine. The Double argument is the particle
-- weight. The particles are sampled in proportion to it.
camerasSample :: [(Double, (GaussianCamera, Map, S.Set Feature))] -> RVar [(ExactCamera, Map, S.Set Feature)]
camerasSample ps = sequence $ replicate (length ps) 
		(particleSample =<< weightedCategorical ps) where
	particleSample (c,m,f) = do
		c' <- cameraSample c
		return (c',m,f)


-- | An EKF camera updated. Implemented according to the original FastSLAM 2.0 
-- paper.
singleFeatureCameraUpdate 
			:: GaussianCamera 
			-> Feature 			-- ^ Must have an associated landmark, otherwise throw an exception
			-> GaussianCamera
singleFeatureCameraUpdate (gcam@(GaussianCamera mu_c cov_c)) feature = let
	landmark = fromJust (flm feature)
	ecam = gauss2exact gcam
	(theta, phi) = fpos feature
		
	_Hl = jacobian_l ecam (lmu landmark)
	_Hc = jacobian_c gcam (lmu landmark)
		
	_Z = _Hl <> lcov landmark <> trans _Hl + measurement_cov -- correct
	cov_c' = inv (trans _Hc <> inv _Z <> _Hc + inv cov_c)
	mu_c'  = cov_c' <> trans _Hc <> inv _Z <> delta_z + mu_c
		
	delta_z = 2 |> [theta `cyclicDiff` z_theta', phi - z_phi'] where
		(z_theta', z_phi') = measure ecam $ lmu landmark
		
	in GaussianCamera mu_c' cov_c'
	

-- | An EKF 'Landmark' update. Implemented according to the original 
-- FastSLAM 2.0 paper. If the feature is not associated to anything, a new
-- 'Landmark' is initialized and inserted into the 'Map'.
singleFeatureLandmarkUpdate :: ExactCamera -> Map -> Feature -> Map
singleFeatureLandmarkUpdate cam m f = case flm f of 
	Nothing -> S.insert (initialize cam f) m
	Just (Landmark id_l mu_l cov_l _ health) -> let
		_H = jacobian_l cam mu_l
		_S = _H <> cov_l <> trans _H + measurement_cov
		_K = cov_l <> trans _H <> inv _S
		_P = (ident 6 - _K <> _H) <> cov_l
		
		(z_theta',z_phi') = measure cam mu_l -- re-projected landmark
		(z_theta, z_phi)  = fpos f		    -- newly observed feature
		
		mu' = mu_l + _K <> (2|> [z_theta `cyclicDiff` z_theta', z_phi - z_phi'])
		cov' = _P
		
		in S.insert (Landmark id_l mu' cov' (descriptor f) (health+(20-health)/10)) m


-- | An EKF 'Map' update routine. It just updates all the features in the 
-- feature set sequentially.
mapUpdate :: ExactCamera -> Map -> S.Set Feature -> Map
mapUpdate cam  m fs = S.fold (flip $ singleFeatureLandmarkUpdate cam) m fs


-- | The main FastSLAM 2.0 update routine. It takes the state from the previous
-- time step and returns the state in the next time step.
-- It is inside the RVar monad, because it contains random processes.
filterUpdate :: [(ExactCamera, Map)]            -- ^ prior state
             -> (ExactCamera -> GaussianCamera) -- ^ transition function
             -> [Feature]                       -- ^ newly observed features
             -> RVar [(ExactCamera, Map)]       -- ^ output state
filterUpdate input_state camTransition features = do 
	let
		feature_set = S.fromList features
		
		--gaussian_mixture :: [(Double, (GaussianCamera, Map, S.Set Feature)]
	gaussian_mixture <- sequence $ do -- each particle in isolation (List Monad)
		(input_camera, input_map) <- input_state
	
		let
			gaussian_proposal :: GaussianCamera
			gaussian_proposal = camTransition input_camera
		
			-- one item in the top-level list for one particle
			pruned_lms :: S.Set Landmark
			pruned_lms = pruneLandmarks input_map
		
		return $ do
		
			lm_list <- shuffle $ S.toList pruned_lms
			let (w, matched_features, updated_camera) =
				updateCamera (-1) lm_list (1, feature_set, gaussian_proposal)
			return (w, (updated_camera, pruned_lms, matched_features))

	-- MAYBE TODO: if stationary, do not update camera positions
	-- resampled_state :: [(ExactCamera, Map, S.Set Feature)]
	resampled_state <- camerasSample gaussian_mixture
	let 
		new_maps :: [Map]
		new_maps = map (\(c,m,f) -> mapUpdate c m f) resampled_state
			
	return $ zip (map (\(ec,_,_) -> ec) resampled_state) new_maps
