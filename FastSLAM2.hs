module FastSLAM2 where

import Numeric.LinearAlgebra
import Data.Random (RVar)
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Categorical (weightedCategorical)
import Data.List (foldl')
import qualified Data.Set as S

import Landmark
import Camera
import Measurement
import InternalMath

import Data.Random
import Data.Random.Source.DevRandom

-- | TODO: tie this with the covariance, defined for observations in Measurement.hs
measurement_cov = diag (2|> [0.01, 0.01])


-- | Find a landmark with a specified ID in a map.
findLm :: LID -> Map -> Maybe Landmark
findLm id lms = if mGE_lm == Just dummyLm then mGE_lm else Nothing where
	dummyLm = Landmark id undefined undefined
	mGE_lm = S.lookupGE dummyLm lms


-- | Implemented according to the original FastSLAM 2.0 paper.
-- TODO: weight calculation
singleFeatureCameraUpdate :: GaussianCamera -> Landmark -> Feature -> (Double, GaussianCamera)
singleFeatureCameraUpdate (gcam@(GaussianCamera mu_c cov_c)) landmark (Feature _ (theta,phi)) = let
	[cx,cy,cz,a,b,g] = toList mu_c
	m2v (a,b) = 2|> [a,b]
	ecam = ExactCamera (3|> [cx,cy,cz]) (euler2rotmat (3|> [a,b,g]))
	
	_Hl = jacobian_l ecam (lmu landmark)
	_Hc = jacobian_c gcam (lmu landmark)
	
	_Z = _Hl <> lcov landmark <> trans _Hl + measurement_cov -- correct
	cov_c = inv (trans _Hc <> inv _Z <> _Hc + inv cov_c)
	mu_c  = cov_c <> trans _Hc <> inv _Z <> ((2|> [theta,phi]) - m2v (measure ecam $ lmu landmark)) + (3|> [cx,cy,cz])
	in (1, GaussianCamera mu_c cov_c)
	
	
-- | TODO: Proper weight calculation
cameraUpdate :: (GaussianCamera, Map) -> [Feature] -> (Double, GaussianCamera)
cameraUpdate (cam, map) fs = foldl' 
	(\(w',c') f -> case findLm (fid f) map of
		Nothing -> (w',c')
		Just lm -> singleFeatureCameraUpdate c' lm f
	) (1, cam) fs


camerasUpdate :: [(GaussianCamera, Map)] -> [Feature] -> [(Double, GaussianCamera)]
camerasUpdate ps fs = map (flip cameraUpdate $ fs) ps


cameraSample :: GaussianCamera -> RVar ExactCamera
cameraSample (GaussianCamera mu cov) = do
	rndVec <- sequence (replicate 6 stdNormal)
	let [cx,cy,cz,a,b,g] = toList $ mu + cov <> (6 |> rndVec)
	return $ ExactCamera (3|> [cx,cy,cz]) (euler2rotmat (3|> [a,b,g]))


-- | TODO: change the function to match the new signature
camerasSample :: [(Double, (GaussianCamera, Map))] -> RVar [(ExactCamera, Map)]
camerasSample ps = sequence $ replicate (length ps) 
		(particleSample =<< weightedCategorical ps) where
	particleSample (c,m) = do
		c' <- cameraSample c
		return (c',m)


singleFeatureLandmarkUpdate :: ExactCamera -> Map -> Feature -> Map
singleFeatureLandmarkUpdate cam m f = case findLm (fid f) m of 
	Just (Landmark id_l mu_l cov_l) -> let
		_H = jacobian_l cam mu_l
		_S = _H <> cov_l <> trans _H + measurement_cov
		_K = cov_l <> trans _H <> inv _S
		_P = (ident 6 - _K <> _H) <> cov_l
		
		m2v (a,b) = 2|> [a,b]
		z' = m2v $ measure cam mu_l -- re-projected landmark
		z = m2v $ fProj f		    -- newly observed feature
		
		mu' = mu_l + _K <> (z-z')
		cov' = _P
		-- | TODO: ?remove the measurement error from this _S coefficient?
		-- | TODO: Rewrite this function to take advantage of the multinormal pdf function written earlier
		--w' = (det (2*pi*_S)) ** (-1/2) * exp ((-1/2 * asRow (z-z') <> inv _S <> asColumn (z-z')) @@> (0,0))
		in S.insert (Landmark id_l mu' cov') m
	Nothing -> S.insert (initialize cam f) $ debug "inserted" m


mapUpdate :: ExactCamera -> Map -> [Feature] -> Map
mapUpdate cam m fs = foldl' (singleFeatureLandmarkUpdate cam) m fs


filterUpdate :: [(ExactCamera, Map)] 
             -> (ExactCamera -> GaussianCamera) 
             -> [Feature] 
             -> RVar [(ExactCamera, Map)]
filterUpdate input_state camTransition features = do 
	let
		input_cameras = map fst input_state
		input_maps = map snd input_state
	
		gaussian_proposals :: [GaussianCamera]
		gaussian_proposals = map camTransition input_cameras
	
		updated_cameras :: [(Double, GaussianCamera)]
		updated_cameras = camerasUpdate (zip gaussian_proposals input_maps) features
	
	-- resampled_state :: [(ExactCamera, Map)]
	resampled_state <- camerasSample $ zipWith (\(a,b) m -> (a,(b,m))) updated_cameras input_maps
	
	let 
		new_maps :: [Map]
		new_maps = map (\(c,m) -> mapUpdate c m features) resampled_state
		
	return $ zip (map fst resampled_state) new_maps
