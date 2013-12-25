-- | 

module EKF2D (initialize, update) where

import Feature
import Data.Matrix hiding (fromList,(!))
import Data.Vector hiding (update)

hg1, hg2 :: Camera2 -> Vector Float -> Float
hg1 (Camera2 pc _) mu = (mu!3) * ((mu!0) - x pc) + sin(mu!2)
hg2 (Camera2 pc _) mu = (mu!3) * ((mu!1) - y pc) + cos(mu!2)

h1, h2 :: Camera2 -> Vector Float -> Float
h1 c@(Camera2 _ phic) f = r11 phic * hg1 c f + r12 phic * hg2 c f
h2 c@(Camera2 _ phic) f = r21 phic * hg1 c f + r22 phic * hg2 c f

r11,r12,r21,r22 :: Float-> Float
r11 = cos
r12 a = -sin a
r21 = sin
r22 = cos

-- | This it the projection of the Feature into the Camera.
-- It doesn't care about covariance
theta :: Camera2 -> Feature -> Float
theta c f = h1 c (mu f) / h2 c (mu f)

-- | Helper jacobianjacob_h :: Camera2 -> Feature4 -> Matrix Float
jacob_h :: Camera2 -> Feature -> Matrix Float
jacob_h (Camera2 (Point2 xc yc) phic) (Feature f _) = fromLists 
	[ [  (f!3)*cos(phic), (f!3)*sin(phic),  cos(phic)*cos(f!2)-sin(phic)*sin(f!2), -(xc-(f!0))*cos(phic)-(yc-(f!1))*sin(phic) ]
    , [ -(f!3)*sin(phic), (f!3)*cos(phic), -cos(f!2)*sin(phic)-cos(phic)*sin(f!2), -(yc-(f!1))*cos(phic)+(xc-(f!0))*sin(phic) ] ]
    
--jacob_h :: Camera2 -> Feature4 -> Matrix Float
--jacob_h (Camera2 (Point2 xc yc) phic) (Feature4 (Point2 xi yi) phii rho) = fromLists 
--	[ [  rho*cos(phic), rho*sin(phic),  cos(phic)*cos(phii)-sin(phic)*sin(phii), -(xc-xi)*cos(phic)-(yc-yi)*sin(phic) ]
--    , [ -rho*sin(phic), rho*cos(phic), -cos(phii)*sin(phic)-cos(phic)*sin(phii), -(yc-yi)*cos(phic)+(xc-xi)*sin(phic) ] ]
    
-- | Jacobian of the 'theta' projection.
jacobian :: Camera2 -> Feature -> Matrix Float
jacobian c f@(Feature mu _) = fromLists [
	[ getElem 1 1 jh / b - absq * getElem 2 1 jh 
	, getElem 1 2 jh / b - absq * getElem 2 2 jh 
	, getElem 1 3 jh / b - absq * getElem 2 3 jh 
	, getElem 1 4 jh / b - absq * getElem 2 4 jh ] ] where
		jh = jacob_h c f
		a = h1 c mu
		b = h2 c mu
		absq = a/b^^2


-- | EKF update routine. It is simplified, so that: 
-- 1. features are statioinary,
-- 2. camera pose is exactly known.
-- implemented as in http://en.wikipedia.org/wiki/Extended_Kalman_Filter
update :: Feature -> Camera2 -> (Float, Float) -> Feature
update (f@(Feature mu cov)) cam (phi, var) = let
--update (f@(Feature4 (Point2 xi yi) phii rho), cov) cam (phi, var) = let
	mu' = mu 					-- state prediction
	cov' = cov				    -- covariance prediction
	f' = Feature mu' cov'
	y = phi - theta cam f'   	-- measurement rezidual
	p = jacobian cam f'
	s = getElem 1 1 (p * cov' * transpose p) + var 	-- rezidual covariance (singleton)
	k = cov' * transpose p * fromLists [[(1/s)]] 	-- gain
	
	in Feature (getCol 1 $ colVector mu + k * fromLists [[y]]) ((identity 4 - k*p)*cov')


-- | Initialize feature from one measurement. Tuple contains mean and variance.
initialize :: Camera2 -> (Float, Float) -> Feature
initialize (Camera2 pos angle) (phi, varphi) = Feature (fromList [x pos, y pos, angle+atan(phi), rho0]) cov where
	rho0 = 0.1
	cov = matrix 4 4 (\(a, b) -> if a /= b then 0 else diag!(a-1))
	diag = fromList [0, 0, abs ((atan(phi-varphi)-atan(phi+varphi))/2), 0.5] :: Vector Float
	
