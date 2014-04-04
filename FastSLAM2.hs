module FastSLAM where

import Data.Random (RVar)
import Data.List (foldl')
--import Data.Random.Distribution.Categorical
import Numeric.LinearAlgebra
import qualified Data.Set as S

import Landmark
import Camera
import Measurement
import InternalMath

-- | TODO: tie this with the covariance, defined for observations in Measurement.hs
measurement_cov = diag (2|> [0.01, 0.01])


-- | Find a landmark with a specified ID in a map.
findLm :: LID -> Map -> Maybe Landmark
findLm id lms = if mGE_lm == Just dummyLm then mGE_lm else Nothing where
	dummyLm = Landmark id undefined undefined
	mGE_lm = S.lookupGE dummyLm lms
			
			
proposal :: ExactCamera -> (Vector Double -> GaussianCamera) -> GaussianCamera
proposal ec = undefined

cameraUpdate :: GaussianCamera -> [Feature] -> (GaussianCamera, Double)
cameraUpdate = undefined

camerasUpdate :: [GaussianCamera] -> [Feature] -> [(GaussianCamera, Double)]
camerasUpdate = undefined
			
cameraSample :: GaussianCamera -> ExactCamera
cameraSample = undefined

camerasSample :: [(Double, GaussianCamera)] -> [ExactCamera]
camerasSample = undefined

		
singleFeatureUpdate :: ExactCamera -> Map -> Feature -> Map
singleFeatureUpdate cam m f = case findLm (fid f) m of 
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
		-- | TODO: ?Refactor this equation to use the RS-SLAM described coefficient?
		-- | TODO: Rewrite this function to take advantage of the multinormal pdf function written earlier
		w' = (det (2*pi*_S)) ** (-1/2) * exp ((-1/2 * asRow (z-z') <> inv _S <> asColumn (z-z')) @@> (0,0))
		in S.insert (Landmark id_l mu' cov') m
	Nothing -> S.insert (initialize cam f) m


mapUpdate :: ExactCamera -> Map -> [Feature] -> Map
mapUpdate cam m fs = foldl' (singleFeatureUpdate cam) m fs

	
{-

-- | A single particle update routine, that is just mapped over in the updateParticles routine.
-- The weights of the resulting particle is properly computed, but it is not
-- normalized against the other existing particles.
updateParticle :: Particle -> [Feature] -> (Camera -> RVar Camera) -> RVar Particle
updateParticle (Particle w cams landmarks) fs h = do
	new_cam <- h (head cams)
	let new_cams = new_cam : cams
	
	-- TODO: compute the particle weight
	
	let (w, new_landmarks) = updateMap new_cam fs landmarks
	return $ Particle w new_cams new_landmarks


-- | The FastSLAM routine, that can just be repeated. Returns properly normalized
-- and resampled particles.
updateParticles :: [Particle] -> [Feature] -> (Camera -> RVar Camera) -> RVar [Particle]
updateParticles ps fs h = do
	new_ps <- sequence (map (\p -> updateParticle p fs h) ps)
	let norm_ps = normalizeWeights new_ps
	when (needResampling norm_ps) resampleParticles' $ norm_ps where
		when b f = if b then f else return


-- | Recursively resample the particles. The particle weights are put one
-- after another to fill the unit interval, then the particles are chosen
-- by equally-spaced points (with spacing (1/length ps)) in the interval.
resampleParticles' :: [Particle] -> RVar [Particle]
resampleParticles' pp = return $ ("nr.parts."`debug`length pp) `seq` resample' (n/2) pp where
	n = 1 / fromIntegral (length pp)
	-- | First argument is the position of the next sampling point in the unit interval
	resample' :: Double -> [Particle] -> [Particle]
	resample' _ [] = []
	resample' i (Particle w c l:ps) = if i < w
			then Particle n c l : resample' (i+n) (Particle w c l:ps)
			else resample' (i-w) ps


-- | The sum of all weights is made to be equal to 1.
normalizeWeights :: [Particle] -> [Particle]
normalizeWeights ss = map (\(Particle w cams ls) -> Particle (w/sumw) cams ls) ss where
		sumw = sum (map weight ss)
-}
