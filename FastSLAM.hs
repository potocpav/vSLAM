module FastSLAM where

import Data.Random (RVar)
--import Data.Random.Distribution.Categorical
--import Numeric.LinearAlgebra
import qualified Data.Set as S

import Feature
import EKF
import InternalMath


type Map = S.Set Landmark
-- | The first number is particle weight, second camera position sequence.
-- The camera sequence should be superfluous for the filter.
data Particle = Particle { weight :: Double, cams :: [Camera], landmarks :: Map }


-- | Find a landmark with a specified ID in a map.
findLm :: LID -> Map -> Maybe Landmark
findLm id lms = if mGE_lm == Just dummyLm then mGE_lm else Nothing where
	dummyLm = Landmark id undefined undefined
	mGE_lm = S.lookupGE dummyLm lms
			
			

updateMap :: Camera -> [Feature] -> Map -> Map
updateMap = undefined where
	singleFeatureUpdate :: Camera -> Feature -> Map -> Map
	singleFeatureUpdate cam f m = case findLm (lID f) m of 
		Just landmark -> undefined -- EKF update existing landmark
		Nothing -> S.insert (initialize cam f) m


-- | A single particle update routine, that is just mapped over in the updateParticles routine.
-- The weights of the resulting particle is properly computed, but it is not
-- normalized against the other existing particles.
updateParticle :: Particle -> [Feature] -> (Camera -> RVar Camera) -> RVar Particle
updateParticle (Particle w cams landmarks) fs h = do
	new_cam <- h (head cams)
	let new_cams = new_cam : cams
	
	-- TODO: compute the particle weight
	
	let new_landmarks = updateMap new_cam fs landmarks
	return $ Particle undefined new_cams new_landmarks


-- | The FastSLAM routine, that can just be repeated. Returns properly normalized
-- and resampled particles.
updateParticles :: [Particle] -> [Feature] -> (Camera -> RVar Camera) -> RVar [Particle]
updateParticles ps fs h = do
	new_ps <- sequence (map (\p -> updateParticle p fs h) ps)
	let norm_ps = normalizeWeights new_ps
	when (needResampling norm_ps) resampleParticles $ norm_ps where
		when b f = if b then f else return


needResampling _ = undefined


resampleParticles _ = undefined


-- | The sum of all weights is made to be equal to 1.
normalizeWeights :: [Particle] -> [Particle]
normalizeWeights ss = map (\(Particle w cams ls) -> Particle (w/sumw) cams ls) ss where
		sumw = sum (map weight ss)
