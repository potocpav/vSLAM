{-# OPTIONS -Wall #-}

module FastSLAM2 where

import Numeric.LinearAlgebra hiding (find, i)
import Data.Random (RVar)
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Categorical (weightedCategorical)
import Data.List (foldl')
import qualified Data.Set as S

import Landmark
import Camera
import Measurement
import InternalMath

-- | TODO: tie this with the covariance, defined for observations in Measurement.hs
measurement_cov :: Matrix Double
measurement_cov = diag (2|> [0.005, 0.005])


-- | Find a landmark with a specified ID in a map.
--findLm :: LID -> Map -> Maybe Landmark
--findLm i lms = if mGE_lm == Just dummyLm then mGE_lm else Nothing where
--	dummyLm = Landmark i undefined undefined undefined
--	mGE_lm = S.lookupGE dummyLm lms
	

-- | Return 2D gaussian of search coordinates in projective space (theta, phi)
searchRegion :: (GaussianCamera, Landmark) -> (Landmark, Gauss)
searchRegion (gcam, lm) = (lm, Gauss mu' cov') where
	ecam = gauss2exact gcam
	mu' = (\(t,p) -> 2|> [t,p]) $ measure ecam (lmu lm)
	
	jc = jacobian_c gcam (lmu lm)
	jl = jacobian_l ecam (lmu lm)
	cov' = jc <> (ccov gcam) <> trans jc + jl <> (lcov lm) <> trans jl + measurement_cov
	
	
-- | Delete the landmarks with insufficient health
pruneLandmarks :: Map -> Map
pruneLandmarks ms = S.map fade $ S.filter (\l -> lhealth l > 0) ms where
	fade lm = lm { lhealth = lhealth lm - 1 }
	

-- | Attempt to guided-match a feature to a given landmark. The result is saved
-- into a matching feature $ set element.
guidedMatch :: (Landmark, Gauss) -> S.Set Feature -> RVar (Double, S.Set Feature)
guidedMatch (lm, g) fs = let
	f_pos f = (\(a,b) -> 2|> [a,b]) $ fpos f
	
	neighbors :: S.Set Feature
	neighbors =  {- ("neigh " ++ show (lid lm)) ++ show (map dist $ S.toList ff) `debug` -} ff where
		ff = S.filter (\f -> dist f < 40 && mahalDist_sq g (f_pos f) < 3*3) fs

	-- | Chose at (weighted) random from the chosen candidates
	updated :: RVar (Maybe Feature)
	updated = return $ if S.size neighbors == 1 then Just $ (head $ S.elems neighbors) {flm = Just lm} else Nothing
	
		-- | Chose at (weighted) random from the chosen candidates
	{-updated :: RVar (Maybe Feature)
	updated = do
		let hypotheses = (0.001, Nothing) : map (\f -> (500-fromIntegral (dist f) :: Double, Just f)) (S.toList neighbors)
		res <- weightedCategorical hypotheses
		return $ case res of
			Nothing -> Nothing
			Just f -> Just $ f {flm = Just lm}
			-}
	
	dist f' = hammingDist (ldescriptor lm) (descriptor f')
	w f = normalDensity g ((\(a,b) -> 2|> [a,b]) (fpos f))
	in do
		upd <- updated
		return $ case upd of
			Nothing -> (1,fs) 
			Just up -> (w up, S.insert up fs)
		
	
	
cameraUpdate :: GaussianCamera -> S.Set Feature -> GaussianCamera
cameraUpdate cam fs = S.foldl' singleFeatureCameraUpdate cam fs


cameraSample :: GaussianCamera -> RVar ExactCamera
cameraSample (GaussianCamera mu cov) = do
	rndVec <- sequence (replicate 6 stdNormal)
	let [cx,cy,cz,a,b,g] = toList $ mu + cov <> (6 |> rndVec)
	return $ ExactCamera (3|> [cx,cy,cz]) (euler2rotmat (3|> [a,b,g]))


camerasSample :: [(Double, (GaussianCamera, Map, S.Set Feature))] -> RVar [(ExactCamera, Map, S.Set Feature)]
camerasSample ps = sequence $ replicate (length ps) 
		(particleSample =<< weightedCategorical ps) where
	particleSample (c,m,f) = do
		c' <- cameraSample c
		return (c',m,f)


-- | Implemented according to the original FastSLAM 2.0 paper.
singleFeatureCameraUpdate :: GaussianCamera -> Feature -> GaussianCamera
singleFeatureCameraUpdate (gcam@(GaussianCamera mu_c cov_c)) feature = {- debug "f" feature `seq` -} case flm feature of
	Nothing -> gcam -- Failed association
	Just landmark -> let
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
	

singleFeatureLandmarkUpdate :: ExactCamera -> Map -> Feature -> Map
singleFeatureLandmarkUpdate cam m f = case flm f of 
	Nothing -> S.insert (initialize cam f) m
	Just (Landmark id_l mu_l cov_l descr health) -> let
		_H = jacobian_l cam mu_l
		_S = _H <> cov_l <> trans _H + measurement_cov
		_K = cov_l <> trans _H <> inv _S
		_P = (ident 6 - _K <> _H) <> cov_l
		
		(z_theta',z_phi') = measure cam mu_l -- re-projected landmark
		(z_theta, z_phi)  = fpos f		    -- newly observed feature
		
		mu' = mu_l + _K <> (2|> [z_theta `cyclicDiff` z_theta', z_phi - z_phi'])
		cov' = _P
		
		in S.insert (Landmark id_l mu' cov' (descriptor f) (health+(20-health)/10)) m


mapUpdate :: ExactCamera -> Map -> S.Set Feature -> Map
mapUpdate cam m fs = S.fold (flip $ singleFeatureLandmarkUpdate cam) m fs


filterUpdate :: [(ExactCamera, Map)] 
             -> (ExactCamera -> GaussianCamera) 
             -> [Feature] 
             -> RVar [(ExactCamera, Map)]
filterUpdate input_state camTransition features = do 
	let
		feature_set = S.fromList $ tail features
		
	--gaussian_mixture :: [(Double, (GaussianCamera, Map, S.Set Feature)]
	gaussian_mixture <- sequence $ do -- each particle in isolation (List Monad)
		(input_camera, input_map) <- input_state
	
		let
			gaussian_proposal :: GaussianCamera
			gaussian_proposal = camTransition input_camera
		
			-- one item in the top-level list for one particle
			pruned_lms :: S.Set Landmark
			pruned_lms = pruneLandmarks input_map
		
			searched_regions :: [(Landmark, Gauss)]
			searched_regions = map (curry searchRegion $ gaussian_proposal) (S.toList pruned_lms)
			
			matched_features :: RVar (Double, S.Set Feature)
			matched_features = foldl'
					(\rv_gm lg -> do
						(w,fs) <- rv_gm
						(gm_w, gm_fs) <- guidedMatch lg fs
						return (w*gm_w, gm_fs)
						)
					(return (1,feature_set)) searched_regions
			
			updated_camera :: RVar GaussianCamera
			updated_camera = do
				mf <- matched_features
				return $ cameraUpdate gaussian_proposal (snd mf)
			
		return $ do
			mfs <- matched_features
			uc <- updated_camera
			return $ (fst mfs, (uc, pruned_lms, snd mfs))

	-- resampled_state :: [(ExactCamera, Map, S.Set Feature)]
	resampled_state <- camerasSample gaussian_mixture
	
	let 
		new_maps :: [Map]
		new_maps = map (\(c,m,f) -> mapUpdate c m f) resampled_state
		
	return $ zip (map (\(ec,_,_) -> ec) resampled_state) new_maps

