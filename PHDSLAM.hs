{-# Language BangPatterns #-}

module PHDSLAM where

import Control.Exception (assert)
import Control.Applicative ((<$>))
import Data.Random (RVar)
import Data.Random.Distribution.Categorical
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
clutter_rate = 0.5

-- | TODO: get a correct value.
_lambda_c = 0.1


-- | Measurement Probability. This is the probability, that a feature would be
-- measured on the sensor in a certain position on projection plane
-- TODO: get a correct value.
_P_D :: Feature -> Camera -> Double
_P_D _ _ = 1 -- We see most landmarks :-)


-- | TODO: tie this with the covariance, defined for the new features in EKF.hs
measurement_cov = diag (2|> [0.01, 0.01])


-- | Compute the pdf of a multivariate normal distribution in the point m.
normalDensity :: Vector Double -> Matrix Double -> Vector Double -> Double
normalDensity mu cov m = norm * exp e where 
	norm = (2*pi)**(-(fromIntegral$dim mu)/2) * (det cov)**(-0.5)
	e = (-0.5) * unpack (asRow(m-mu) <> inv cov <> asColumn(m-mu))
	unpack m = assert (rows m == 1 && cols m == 1) $ m @@> (0,0)


-- | Predict step fuses the measurements and existing features into one list.
-- It also propagates Camera position a step further, according to a probabilistic
-- transition function supplied.
-- The last return value is a number, that is needed in particle weighting routine.
predict :: [Camera] -> [[Measurement]] -> [Feature] -> ([Camera] -> RVar Camera) -> RVar (Camera, [Feature], Double)
predict cams (m:[]) fs f = predict cams (m:[[]]) fs f
predict cams (m@(_:ms:_)) fs f = do
	-- | initialize using the old position and measurements
	let new_features = map (\a -> initialize (head cams) a) ms
	cam' <- f cams
	let m_kk1 = sum $ map eta (new_features ++ fs)
	return (cam', new_features ++ fs, m_kk1)


missedDetections :: Camera -> [Feature] -> [Feature]
missedDetections cam =
	map $ \(f@(Feature _eta _mu _P)) -> Feature ((1 - _P_D f cam) * _eta) _mu _P


-- | Tail-recursive routine, producing the terms needed for the EKF update.
updateTerms :: Camera -> [Feature] -> [UpdateTerms]
updateTerms cam (Feature _ mu cov:fs) = Terms _z' _S _K _P : updateTerms cam fs where
	_z' = measure cam mu
	_H = jacobian cam mu
	_S = _H <> cov <> trans _H + measurement_cov
	_K = cov <> trans _H <> inv _S
	_P = (ident 6 - _K <> _H) <> cov	
updateTerms _ [] = []


-- | Merging and pruning operations.
-- It is rather simple  now (only pruning) and could use some improvement :-)
prune :: [Feature] -> [Feature]
prune f = filter (\x -> eta x > 10^^(-3)) f


detection :: Camera -> [UpdateTerms] -> Measurement -> [Feature] -> [Feature]
detection cam u m f = normalize $ detection' u m f where

	normalize :: [Feature] -> [Feature]
	normalize fs = map (\(Feature tau mu cov) -> Feature (tau / sumf) mu cov) fs where
		sumf = clutter_rate + ("sum"`debug`sum (map eta fs))
	
	-- | Function, that outputs Features with not-normalized weights tau'.
	detection' :: [UpdateTerms] -> Measurement -> [Feature] -> [Feature]
	detection' (Terms _z' _S _K _P : rest) m (f : fs) = let
		m2v (a,b) = 2|> [a,b]
		tau' = _P_D f cam * eta f * (normalDensity (m2v _z') _S (m2v m))
		mu' = mu f + _K <> (m2v m - m2v _z')
		cov' = _P
		in Feature tau' mu' cov' : detection' rest m fs
	detection' [] _ [] = []
	

mapUpdate :: Camera -> [Measurement] -> [Feature] -> ([Feature], Double)
mapUpdate cam ms fs = (prune detections, m_k') where
	us = updateTerms cam fs
	
	-- Missed detections ++ Realized detections
	detections = missedDetections cam fs 
		++ concat (map (\m -> detection cam us m fs) ms)
	
	m_k' = sum $ map eta detections


-- | Zero-feature single-particle update routine.
updateParticle0 :: [[Measurement]] -> Particle -> ([Camera] -> RVar Camera) -> RVar Particle
updateParticle0 (m@(ms:_)) (w_k1, cams, fs) f = do
	(cam', fs', m_kk1) <- predict cams m fs f
	let (fs'', m_k) = mapUpdate cam' ms fs'
	let w_k = ((clutter_rate ^^ length ms) * exp ((m_k) - (m_kk1) - (_lambda_c))) * w_k1
	return (w_k, cam':cams, fs'')
	
	
-- | Single-feature single-particle update routine.
-- TODO: Implement.
updateParticle1 :: [[Measurement]] -> Particle -> ([Camera] -> RVar Camera) -> RVar Particle
updateParticle1 (m@(ms:_)) (w_k1, cams, fs) f = do
	(cam', fs', m_kk1) <- predict cams m fs f
	let (fs'', m_k) = mapUpdate cam' ms fs'
	
	let chosen = getBiggest Nothing fs'' where
		getBiggest :: Maybe Feature -> [Feature] -> Feature
		getBiggest Nothing (f:fs) = getBiggest (Just f) fs
		getBiggest (Just m) (f:fs) = getBiggest (Just (if eta m > eta f then m else f)) fs
		getBiggest (Just m) [] = m
		
	let w_k = a/b * w_k1 where
		a = (1 - _P_D undefined undefined)*(clutter_rate ^^ length ms) +
			_P_D undefined undefined * undefined
		b = (clutter_rate ^^ length ms)
		
	return (w_k, cams, fs)
	


-- | The whole RB-PHD-SLAM routine.
-- Step in time: do the whole EKF update, and then resample the particles
updateParticles :: [[Measurement]] -> [Particle] -> ([Camera] -> RVar Camera) -> RVar [Particle]
updateParticles mss ps f = do
	ups <- sequence (map (\p -> updateParticle0 mss p f) ps)
	let unps = normalizeWeights ups
	when (needResampling unps) resampleParticles' $ unps where
		when b f = if b then f else return


-- | The sum of all weights is made to be equal to 1.
normalizeWeights :: [Particle] -> [Particle]
normalizeWeights ss = map (\(w,r,s) -> (w/sumw,r,s)) ss where
		sumw = sum (map (\(w,_,_)->w) ss)
		

-- | Get the number of effective particles and resample, if it is lower than the treshold.
-- needResampling :: [Particle] -> Bool
needResampling ps = debug "Eff.no.of.particles" (1 / sum (map (\(w,_,_) -> w*w) ps)) < treshold where
	treshold = 15


-- | Resampling step. It draws in random from the particle pool
resampleParticles :: [Particle] -> RVar [Particle]
resampleParticles ps =  sequence . replicate len . categorical 
	$ map (\(w,r,t)->(w,(1/fromIntegral len,r,t))) ps where
	len = length ps

-- | Recursively resample the particles. The particle weights are put one
-- after another to fill the unit interval, then the particles are chosen
-- by equally-spaced points (with spacing (1/length ps)) in the interval.
resampleParticles' :: [Particle] -> RVar [Particle]
resampleParticles' pp = return $ resample' (n/2) pp where
	n = 1 / fromIntegral (length pp)
	-- | First argument is the position of the next sampling point in the unit interval
	resample' :: Double -> [Particle] -> [Particle]
	resample' _ [] = []
	resample' i ((w,r,t):ps) = if i < w + n
			then (n,r,t) : resample' (i+n) ((w,r,t):ps)
			else resample' (i-w) ps

