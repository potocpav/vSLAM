{-# Language BangPatterns #-}

module PHDSLAM where

import Control.Exception (assert)
import Control.Applicative ((<$>))
import Data.Random (RVar)
import Numeric.LinearAlgebra

import Feature
import EKF
import InternalMath


-- | Just a helper structure (could be a tuple instead).
data UpdateTerms = Terms 
	Measurement -- ^ z'
	(Matrix Double) -- ^ S
	(Matrix Double) -- ^ K
	(Matrix Double) -- ^ P
	

-- | The first number is particle weight, second camera position sequence
type Particle = (Double, [Camera], [Feature])


-- | The probability of a false alarm on a sensor. As of now, it is independent 
-- of Measurement. We will see if it is cool or not.
-- If it is independent, then it is probably equal to lambda_c
clutter_rate :: Double
clutter_rate = 0.1

-- | TODO: get a correct value.
_lambda_c = 0.1


-- | Measurement Probability. This is the probability, that a feature would be
-- measured on the sensor in a certain position on projection plane
-- TODO: get a correct value.
_P_D :: Feature -> Camera -> Double
_P_D _ _ = 1.0 -- We see EEeeeverything :-)


-- | TODO: tie this with the covariance, defined for the new features in EKF.hs
measurement_cov = diag (2|> [0.01, 0.01])


-- | Compute the pdf of a multivariate normal distribution in the point m.
normalDensity :: Vector Double -> Matrix Double -> Vector Double -> Double
normalDensity mu cov m = norm * exp e where 
	norm = (2*pi)**(-(fromIntegral$dim mu)/2) * (det cov)**(-0.5)
	e = ((-0.5) * unpack (asRow(m-mu) <> inv cov <> asColumn(m-mu)))
	unpack m = assert (rows m == 1 && cols m == 1) $ m @@> (0,0)


-- | Predict step fuses the measurements and existing features into one list.
-- It also propagates Camera position a step further, according to a probabilistic
-- transition function supplied.
-- The last return value is a number, that is needed in particle weighting routine.
predict :: [Camera] -> [Measurement] -> [Feature] -> ([Camera] -> RVar Camera) -> RVar (Camera, [Feature], Double)
predict cams ms fs f = do
	cam' <- f cams
	let new_features = map (\a -> initialize cam' a) ms
	let m_kk1 = sum $ map eta new_features
	return (cam', new_features ++ fs, m_kk1)


missedDetections :: Camera -> [Feature] -> [Feature]
missedDetections cam =
	map $ \(f@(Feature _eta _mu _P)) -> Feature ((1 - _P_D f cam) * _eta) _mu _P


-- | Tail-recursive routine, producing the terms needed for the EKF update.
updateTerms :: Camera -> [Feature] -> [UpdateTerms]
updateTerms cam (f@(Feature _ mu cov):fs) = Terms _z' _S _K _P : updateTerms cam fs where
	_z' = measure cam mu
	_H = jacobian cam mu
	_S = _H <> cov <> trans _H + measurement_cov
	_K = cov <> trans _H <> inv _S
	_P = (ident 6 - _K <> _H) * cov	
updateTerms _ _ = []


-- | Merging and pruning operations.
-- It is rather simple  now (only pruning) and could use some improvement :-)
prune :: [Feature] -> [Feature]
prune f = filter (\x -> eta x > 10^^(-3)) f


detection :: Camera -> [UpdateTerms] -> Measurement -> [Feature] -> [Feature]
detection cam u m f = normalize $ detection' u m f where

	normalize :: [Feature] -> [Feature]
	normalize fs = map (\(Feature tau mu cov) -> Feature (tau / sumf) mu cov) fs where
		sumf = clutter_rate + sum (map eta fs)
	
	-- | Function, that outputs Features with not-normalized weights tau'.
	detection' :: [UpdateTerms] -> Measurement -> [Feature] -> [Feature]
	detection' (Terms _z' _S _K _P : rest) m (f : fs) = let
		m2v (a,b) = 2|> [a,b]
		tau' = _P_D f cam * eta f * (normalDensity (m2v _z') _S (m2v m))
		mu' = mu f + _K <> (m2v m - m2v _z')
		cov' = _P
		in  Feature tau' mu' cov' : detection' rest m fs
	detection' [] _ [] = []
	

mapUpdate :: Camera -> [Measurement] -> [Feature] -> ([Feature], Double)
mapUpdate cam ms fs = (prune detections, m_k') where
	us = updateTerms cam fs
	
	-- Missed detections ++ Realized detections
	detections = missedDetections cam fs 
		++ concat (map (\m -> detection cam us m fs) ms)
	
	m_k' = sum $ map eta detections


-- | Zero-feature single-particle update routine.
updateParticle :: [Measurement] -> Particle -> ([Camera] -> RVar Camera) -> RVar Particle
updateParticle ms (w_k1, cams, fs) f = do
	(cam', fs', m_kk1) <- predict cams ms fs f
	let (fs'', m_k) = mapUpdate cam' ms fs'
	let w_k = (clutter_rate ^^ length ms * exp (m_k - m_kk1 - _lambda_c)) * w_k1
	return (w_k, cam':cams, fs'')
	

-- | The sum of all weights is equal to 1.
normalizeWeights :: [Particle] -> [Particle]
normalizeWeights ss = map (\(w,r,s) -> (w/sumw,r,s)) ss where
	sumw = sum (map (\(w,_,_)->w) ss)


-- | The whole RB-PHD-SLAM routine.
-- Step in time: do the whole EKF update, and then resample the particles
updateParticles :: [Measurement] -> [Particle] -> ([Camera] -> RVar Camera) -> RVar [Particle]
updateParticles ms ps f = (resampleParticles.normalizeWeights) <$> 
		sequence (map (\p -> updateParticle ms p f) ps)


-- | Resampling step. This could use some improvement :-) Identity alias is
-- clearly not this function's purpose...
resampleParticles :: [Particle] -> [Particle]
resampleParticles = map (\(n,(!a,!b,!c)) -> (("w"++show n)`debug`a,b,c)) . (zip [1..])
