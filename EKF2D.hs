-- | 

module EKF2D (initialize, update, theta, jacobian) where

import Feature
import qualified Data.Matrix as M
import qualified Data.Vector as V

(!) = (V.!)

hg1, hg2 :: Camera -> V.Vector Float -> Float
hg1 (Camera (pcx, _) _) mu = (mu!3) * ((mu!0) - pcx) + sin(mu!2)
hg2 (Camera (_, pcy) _) mu = (mu!3) * ((mu!1) - pcy) + cos(mu!2)

h1, h2 :: Camera -> V.Vector Float -> Float
h1 c@(Camera _ phic) f = r11 phic * hg1 c f + r12 phic * hg2 c f
h2 c@(Camera _ phic) f = r21 phic * hg1 c f + r22 phic * hg2 c f

r11,r12,r21,r22 :: Float-> Float
r11 = cos
r12 a = -sin a
r21 = sin
r22 = cos

-- | This it the projection of the Feature into the Camera.
-- It doesn't care about covariance
theta :: Camera -> V.Vector Float -> Float
theta c mu = h1 c mu / h2 c mu

-- | Helper jacobian. Takes camera and mean value of a feature, returns the jacobian
-- of measurement function, before roatation.
jacob_h :: Camera -> V.Vector Float -> M.Matrix Float
jacob_h (Camera (xc, yc) phic) f = M.fromLists 
	[ [  (f!3)*cos(phic), (f!3)*sin(phic),  cos(phic)*cos(f!2)-sin(phic)*sin(f!2), -(xc-(f!0))*cos(phic)-(yc-(f!1))*sin(phic) ]
    , [ -(f!3)*sin(phic), (f!3)*cos(phic), -cos(f!2)*sin(phic)-cos(phic)*sin(f!2), -(yc-(f!1))*cos(phic)+(xc-(f!0))*sin(phic) ] ]
    

-- | Jacobian of the 'theta' projection.
-- Takes camera and the mean value of a feature.
jacobian :: Camera -> V.Vector Float -> M.Matrix Float
jacobian c mu = M.fromLists [
	[ M.getElem 1 1 jh / b - absq * M.getElem 2 1 jh 
	, M.getElem 1 2 jh / b - absq * M.getElem 2 2 jh 
	, M.getElem 1 3 jh / b - absq * M.getElem 2 3 jh 
	, M.getElem 1 4 jh / b - absq * M.getElem 2 4 jh ] ] where
		jh = jacob_h c mu
		a = h1 c mu
		b = h2 c mu
		absq = a/b^^2


-- | EKF update routine. It is simplified, so that: 
-- 1. features are statioinary,
-- 2. camera pose is exactly known.
-- implemented as in http://en.wikipedia.org/wiki/Extended_Kalman_Filter
-- TODO: delete this one!
update :: Feature -> Camera -> (Float, Float) -> Feature
update (f@(Feature eta mu cov)) cam (phi, var) = let
	mu' = mu 					-- state prediction
	cov' = cov				    -- covariance prediction
	f' = Feature eta mu' cov'
	y = phi - theta cam mu   	-- measurement rezidual
	p = jacobian cam mu
	s = M.getElem 1 1 (p * cov' * M.transpose p) + var 	-- rezidual covariance (singleton)
	k = cov' * M.transpose p * M.fromLists [[(1/s)]] 	-- gain
	
	in Feature eta (M.getCol 1 $ M.colVector mu + k * M.fromLists [[y]]) ((M.identity 4 - k*p)*cov')


-- | Initialize feature from one measurement. Tuple contains mean and variance.
-- It initialises also the feature weight. But i do not know, what value is desired.
-- TODO: tweak the eta value (maybe not 1.0).
initialize :: Camera -> (Float, Float) -> Feature
initialize (Camera (posx,posy) angle) (phi, varphi) = Feature 0.02 (V.fromList [posx, posy, angle+atan(phi), rho0]) cov where
	rho0 = 0.1
	cov = M.matrix 4 4 (\(a, b) -> if a /= b then 0 else diag!(a-1))
	diag = V.fromList [0, 0, abs ((atan(phi-varphi)-atan(phi+varphi))/2), 0.5] :: V.Vector Float
	
