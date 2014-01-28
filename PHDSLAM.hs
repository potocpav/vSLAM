
module PHDSLAM where

import EKF2D
import Feature
import Simulate (measure)
import qualified Data.Matrix as M
import qualified Data.Vector as V
import GHC.Float (double2Float, float2Double)

import Statistics.Distribution (density)
import Statistics.Distribution.Normal (normalDistr)
import Data.RVar
import Control.Applicative ((<$>))

-- TODO: Consider replacing lists by vectors. It could be more efficient.

-- | Just a helper structure (could be a tuple instead).
data UpdateTerms = Terms 
	Float -- ^ z'
	(M.Matrix Float) -- ^ S
	(M.Matrix Float) -- ^ K
	(M.Matrix Float) -- ^ P
	
type Particle = (Float, Camera, [Feature])

-- | Take a normal distribution (in a detector-space) and compute the density
-- function f(m;mu,cov).
normalDensity :: Float -> Float -> Float -> Float
normalDensity mu cov m = d2f $ density (normalDistr (f2d mu) (f2d $ sqrt cov)) (f2d m)
	where d2f = double2Float
	      f2d = float2Double

-- | Measurement covariance. In practice would be something like the angle of 1 pixel.
measurement_cov = M.fromLists [[0.1]]

-- | TODO: get a correct value
_lambda_c = 0.1

-- | Measurement Likelihood. This is the probability, that a feature would be
-- measured on the sensor in a certain position on projection plane
_P_D :: V.Vector Float -> Camera -> Float
_P_D mu cam = if measure cam (mu V.! 0, mu V.! 1) == Nothing then 0 else 0.9

-- | Clutter Rate; this is the probability of a false alarm on a sensor.
-- This is independent of Measurement, we will see if it is cool or not
-- If it is independent, then it is probably equal to lambda_c
_c :: Float
_c = 0.1

-- | Predict step fuses the measurements and existing features into one list.
-- It also propagates Camera position a step further, according to a probabilistic
-- transition function supplied.
predict :: Camera -> [Measurement] -> [Feature] -> (Camera -> RVar Camera) -> RVar (Camera, [Feature], Float)
predict cam ms fs f = do
	cam' <- f cam
	let features' = map (\a -> initialize cam' (a,0.1)) ms ++ fs
	let m_kk1 = sum $ map eta features'
	return (cam', features', m_kk1)

missedDetections :: Camera -> [Feature] -> [Feature]
missedDetections cam = 
	map $ \(Feature _eta _mu _P) -> Feature ((1 - _P_D _mu cam) * _eta) _mu _P


-- | Tail-recursive routine, producing the terms needed for the EKF update.
updateTerms :: Camera -> [Feature] -> [UpdateTerms]
updateTerms cam (f@(Feature _ mu cov):fs) = Terms _z' _S _K _P : updateTerms cam fs where
	_z' = theta cam mu
	_H = jacobian cam mu
	_S = _H * cov * M.transpose _H + measurement_cov
	_K = cov * M.transpose _H * (M.fromLists [[1 / M.getElem 1 1 _S]])
	_P = (M.identity 4 - _K * _H) * cov	
updateTerms _ _ = []


detection :: Camera -> [UpdateTerms] -> Measurement -> [Feature] -> [Feature]
detection cam u m f = normalize $ detection' u m f where

	normalize :: [Feature] -> [Feature]
	normalize fs = map (\(Feature tau mu cov) -> Feature (tau / sumf) mu cov) fs where
		sumf = sum $ map eta fs
	
	-- | Function, that outputs Features with not-normalized weights tau'.
	detection' :: [UpdateTerms] -> Measurement -> [Feature] -> [Feature]
	detection' (Terms _z' _S _K _P : rest) m (f : fs) = let
		tau' = _P_D (mu f) cam * (eta f) * normalDensity _z' (M.getElem 1 1 _S) m
		mu' = M.getCol 1 $ (M.colVector $ mu f) + _K * M.fromLists [[m - _z']]
		cov' = _P
		in  Feature tau' mu' cov' : detection' rest m fs
	detection' _ _ _ = []


-- | Merging and pruning operations.
-- As you can see, it is rather simple and could use some improvement :-)
prune :: [Feature] -> [Feature]
prune = id

-- | TODO: Pruning and Merging operations.
mapUpdate :: Camera -> [Measurement] -> [Feature] -> ([Feature],Float)
mapUpdate cam ms fs = (prune detections, m_k') where
	us = updateTerms cam fs
	
	-- Missed detections ++ Realized detections
	detections = missedDetections cam fs 
		++ concat (map (\m -> detection cam us m fs) ms)
	
	m_k' = sum $ map eta detections


-- | Zero-feature single-particle update routine.
updateParticle :: [Measurement] -> Particle -> (Camera -> RVar Camera) -> RVar Particle
updateParticle ms (w_k1, cam, fs) f = do
	(cam', fs', m_kk1) <- predict cam ms fs f
	let (fs'', m_k) = mapUpdate cam' ms fs'
	let w_k = (_c ^^ length ms * exp (m_k - m_kk1 - _lambda_c)) * w_k1
	return (w_k, cam', fs'')


-- | The whole RB-PHD-SLAM routine.
-- Step in time: do the whole EKF update, and then resample the particles
updateParticles :: [Measurement] -> [Particle] -> (Camera -> RVar Camera) -> RVar [Particle]
updateParticles ms ps f = resampleParticles <$> sequence (map (\p -> updateParticle ms p f) ps)


-- | Resampling step. This could use some improvement :-) Identity alias is
-- clearly not this function's purpose...
resampleParticles :: [Particle] -> [Particle]
resampleParticles = id
