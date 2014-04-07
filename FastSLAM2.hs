{-# OPTIONS -Wall #-}

module FastSLAM2 where

import Numeric.LinearAlgebra hiding (find)
import Data.Random (RVar)
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Categorical (weightedCategorical)
import Data.List (foldl', find)
import qualified Data.Set as S

import Landmark
import Camera
import Measurement
import InternalMath

-- | TODO: tie this with the covariance, defined for observations in Measurement.hs
measurement_cov :: Matrix Double
measurement_cov = diag (2|> [0.001, 0.001])


-- | Find a landmark with a specified ID in a map.
findLm :: LID -> Map -> Maybe Landmark
findLm id lms = if mGE_lm == Just dummyLm then mGE_lm else Nothing where
	dummyLm = Landmark id undefined undefined
	mGE_lm = S.lookupGE dummyLm lms
	

-- | Return 2D gaussian of search coordinates in projective space (theta, phi)
searchRegion :: (GaussianCamera, Landmark) -> (LID, Gauss)
searchRegion (gcam@(GaussianCamera mu_c cov_c), Landmark i mu_l cov_l) = (i, Gauss mu' cov') where
	ecam = gauss2exact gcam
	mu' = (\(t,p) -> 2|> [t,p]) $ measure ecam mu_l
	
	jc = jacobian_c gcam mu_l
	jl = jacobian_l ecam mu_l
	cov' = jc * cov_c * trans jc + jl * cov_l * trans jl + measurement_cov
	
	
-- | Chose all landmarks present
choseLandmarks :: Map -> [Landmark]
choseLandmarks ms = S.toList ms
	

-- | For now, ignoring the gaussian and performing exhaustive search for the
-- matching LID
guidedMatch :: (LID, Gauss) -> [Feature] -> Maybe (Double, Feature)
guidedMatch (i, Gauss mu' cov') fs = do
	feature <- find (\f -> fid f == i) fs
	return (1, feature)
	
	
-- | TODO: Proper weight calculation
cameraUpdate :: GaussianCamera -> [(Landmark, Maybe (Double, Feature))] -> (Double, GaussianCamera)
cameraUpdate cam lfs = foldl' 
	(\(w',c') (l, mfs) -> case mfs of
		Nothing -> (w',c')
		Just (w, f) -> (w' * w, singleFeatureCameraUpdate c' l f)
	) (1, cam) lfs


cameraSample :: GaussianCamera -> RVar ExactCamera
cameraSample (GaussianCamera mu cov) = do
	rndVec <- sequence (replicate 6 stdNormal)
	let [cx,cy,cz,a,b,g] = toList $ mu + cov <> (6 |> rndVec)
	return $ ExactCamera (3|> [cx,cy,cz]) (euler2rotmat (3|> [a,b,g]))


camerasSample :: [(Double, (GaussianCamera, Map))] -> RVar [(ExactCamera, Map)]
camerasSample ps = sequence $ replicate (length ps) 
		(particleSample =<< weightedCategorical ps) where
	particleSample (c,m) = do
		c' <- cameraSample c
		return (c',m)


-- | Implemented according to the original FastSLAM 2.0 paper.
singleFeatureCameraUpdate :: GaussianCamera -> Landmark -> Feature -> GaussianCamera
singleFeatureCameraUpdate (gcam@(GaussianCamera mu_c cov_c)) landmark (Feature _ (theta,phi)) = let
	ecam = gauss2exact gcam
	
	_Hl = jacobian_l ecam (lmu landmark)
	_Hc = jacobian_c gcam (lmu landmark)
	
	_Z = _Hl <> lcov landmark <> trans _Hl + measurement_cov -- correct
	cov_c' = inv (trans _Hc <> inv _Z <> _Hc + inv cov_c)
	mu_c'  = cov_c' <> trans _Hc <> inv _Z <> delta_z + mu_c
	
	delta_z = 2 |> [theta `cyclicDiff` z_theta', phi - z_phi'] where
		(z_theta', z_phi') = measure ecam $ lmu landmark
	
	in GaussianCamera mu_c' cov_c'
	

singleFeatureLandmarkUpdate :: ExactCamera -> Map -> Feature -> Map
singleFeatureLandmarkUpdate cam m f = case findLm (fid f) m of 
	Just (Landmark id_l mu_l cov_l) -> let
		_H = jacobian_l cam mu_l
		_S = _H <> cov_l <> trans _H + measurement_cov
		_K = cov_l <> trans _H <> inv _S
		_P = (ident 6 - _K <> _H) <> cov_l
		
		(z_theta',z_phi') = measure cam mu_l -- re-projected landmark
		(z_theta, z_phi)  = fProj f		    -- newly observed feature
		
		mu' = mu_l + _K <> (2|> [z_theta `cyclicDiff` z_theta', z_phi - z_phi'])
			
		cov' = _P
		-- | TODO: ?remove the measurement error from this _S coefficient?
		-- | TODO: Rewrite this function to take advantage of the multinormal pdf function written earlier
		--w' = (det (2*pi*_S)) ** (-1/2) * exp ((-1/2 * asRow (z-z') <> inv _S <> asColumn (z-z')) @@> (0,0))
		in S.insert (Landmark id_l mu' cov') m
	Nothing -> S.insert (initialize cam f) m


mapUpdate :: ExactCamera -> Map -> [Feature] -> Map
mapUpdate cam m fs = foldl' (singleFeatureLandmarkUpdate cam) m fs


filterUpdate :: [(ExactCamera, Map)] 
             -> (ExactCamera -> GaussianCamera) 
             -> [Feature] 
             -> RVar [(ExactCamera, Map)]
filterUpdate input_state camTransition features = do 
	let
		--gaussian_mixture :: [(Double, (GaussianCamera, Map), [(Maybe LID, Feature)])]
		gaussian_mixture = do -- each particle in isolation (List Monad)
		  (input_camera, input_map) <- input_state
	
		  let
			gaussian_proposal :: GaussianCamera
			gaussian_proposal = camTransition input_camera
		
			-- one item in the top-level list for one particle
			searched_lms :: [Landmark]
			searched_lms = choseLandmarks input_map
		
			searched_regions :: [(LID, Gauss)]
			searched_regions = map (curry searchRegion $ gaussian_proposal) searched_lms
			
			matched_features :: [Maybe (Double, Feature)]
			matched_features = map ((flip guidedMatch) features) searched_regions
			
			updated_camera :: (Double, GaussianCamera)
			updated_camera = cameraUpdate gaussian_proposal (zip searched_lms matched_features)
			
		  return $ (\(w,c) -> (w, (c, input_map))) updated_camera

	-- resampled_state :: [(ExactCamera, Map)]
	resampled_state <- camerasSample gaussian_mixture
	
	let 
		new_maps :: [Map]
		new_maps = map (\(c,m) -> mapUpdate c m features) resampled_state
		
	return $ zip (map fst resampled_state) new_maps

