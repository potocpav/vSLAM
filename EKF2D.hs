-- | 

module EKF2D (update) where

import Feature
import Data.Matrix

hg1 (Camera2 pc _) (Feature4 pi phii rho) = rho * (x pi - x pc) + sin(phii)
hg2 (Camera2 pc _) (Feature4 pi phii rho) = rho * (y pi - y pc) + cos(phii)

h1 c@(Camera2 _ phic) f = r11 phic * hg1 c f + r12 phic * hg2 c f
h2 c@(Camera2 _ phic) f = r21 phic * hg1 c f + r22 phic * hg2 c f

r11 phi = cos phi
r12 phi = sin phi
r21 phi = -sin phi
r22 phi = cos phi

theta :: Camera2 -> Feature4 -> Float
theta c f = h1 c f / h2 c f

jacob_h :: Camera2 -> Feature4 -> Matrix Float
jacob_h (Camera2 (Point2 xc yc) phic) (Feature4 (Point2 xi yi) phii rho) = fromLists 
	[ [  rho*cos(phic), rho*sin(phic),  cos(phic)*cos(phii)-sin(phic)*sin(phii), -(xc-xi)*cos(phic)-(yc-yi)*sin(phic) ]
    , [ -rho*sin(phic), rho*cos(phic), -cos(phii)*sin(phic)-cos(phic)*sin(phii), -(yc-yi)*cos(phic)+(xc-xi)*sin(phic) ] ]
    

jacobian :: Camera2 -> Feature4 -> Matrix Float
jacobian c f = fromLists [
	[ getElem 1 1 jh / b - absq * getElem 2 1 jh 
	, getElem 1 2 jh / b - absq * getElem 2 2 jh 
	, getElem 1 3 jh / b - absq * getElem 2 3 jh 
	, getElem 1 4 jh / b - absq * getElem 2 4 jh ] ] where
		jh = jacob_h c f
		a = h1 c f
		b = h2 c f
		absq = a/b^^2


-- | EKF update routine. It is simplified, so that: 
-- 1. features are statioinary,
-- 2. camera pose is exactly known.
-- implemented as in http://en.wikipedia.org/wiki/Extended_Kalman_Filter
update :: (Feature4, Cov4) -> Camera2 -> (Float, Float) -> (Feature4, Cov4)
update (f@(Feature4 (Point2 xi yi) phii rho), cov) cam (phi, var) = let
	f' = f 					-- state prediction
	cov' = cov				-- covariance prediction
	y = phi - theta cam f' 	-- measurement rezidual
	p = jacobian cam f'
	s = getElem 1 1 (p * cov' * transpose p) + var 	-- rezidual covariance (singleton)
	k = cov' * transpose p * fromLists [[(1/s)]] 	-- gain
	
	-- todo: update state estimate
	in (Feature4 (Point2 (xi+k!(1,1)) (yi+k!(2,1))) (phii+k!(3,1)) (rho+k!(4,1)), (identity 4 - k*p)*cov)
