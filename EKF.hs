
-- | Now this is a somehow-well-tested library.

-- TODO: Clean export list. Right now some modules are needed just by the unit test.
module EKF (measure, jacobian, initialize) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util ((!))
import Feature
import InternalMath


--------------------------------------------------------------------------------
-- Feature initialization parameters

-- | Could be dependent on the position of a measurement sensor.
-- | TODO: tie this with the covariance, defined for observations in PHDSLAM.hs
initialCov :: Matrix Double
initialCov = diag(6|> [0,0,0,0.01,0.01,0.5])

-- | TODO: check the correctness of this value (10 or 1/10 or other?)
initialRho = 0.2

--------------------------------------------------------------------------------


-- | 6D Landmark mean to a directional (un-normalized) vector in camera-space.
-- The first parameter is the camera position, the second is the landmark mean.
-- The result is (obviously) independent of camera rotation.
measure_g :: Vector Double -> Vector Double -> Vector Double
measure_g camPos f = scale rho (fpos - camPos) + euler2vec (theta, phi) where
	[fpos, tmp] = takesV [3,3] f
	[theta, phi, rho] = toList tmp

-- | 6D Landmark mean to a directional (un-normalized) vector in world-space.
measure_h :: Camera -> Vector Double -> Vector Double
measure_h (Camera cp cr) f = head.toColumns $ trans cr <> asColumn (measure_g cp f)

-- | The projection of the 6D Landmark mean into theta-phi parametrisation.
measure :: Camera -> Vector Double -> (Double, Double)
measure c f = vec2euler $ measure_h c f


-- | Measurement equation jacobian with respect to the feature
jacobian :: Camera -> Vector Double -> Matrix Double
jacobian (cam@(Camera cp cr)) f = fromRows [e1', e2'] where
	[x, y, z, theta, phi, rho] = toList f
	[x_c, y_c, z_c] = toList cp
	[h_x, h_y, h_z] = toList $ measure_h cam f
	[h_x', h_y', h_z'] = toRows h'
	h' = trans cr <> ((3><6) 
		[ rho,   0,   0,  cos theta * cos phi, -sin theta * sin phi, x - x_c
		,   0, rho,   0,                    0,             -cos phi, y - y_c
		,   0,   0, rho, -sin theta * cos phi, -cos theta * sin phi, z - z_c ])
	
	xxzz = h_x*h_x + h_z*h_z
	e1' = scale (h_z / xxzz) h_x' - scale (h_x / xxzz) h_z'
	e2' = scale (-sqrt xxzz / (xxzz + h_y*h_y)) h_y' +
	      scale (h_y / (xxzz + h_y*h_y) / sqrt xxzz) 
			(scale h_x h_x' + scale h_z h_z')



-- | Measurement equation jacobian with respect to the camera
jacobian_c :: Camera -> Vector Double -> Matrix Double
jacobian_c (cam@(Camera cp cr)) f = fromRows [e1', e2'] where
	[x, y, z, theta, phi, rho] = toList f
	[x_c, y_c, z_c] = toList cp
	-- [h_x, h_y, h_z] = toList $ measure_h cam f
	--[h_x', h_y', h_z'] = toRows h'

	dR_da = undefined :: Matrix Double
	dR_db = undefined :: Matrix Double
	dR_dg = undefined :: Matrix Double
	g = measure_g cp f
	--h' = (cr <> scale (-rho) (ident 3)) ! (dR_da <> g) ! (dR_db <> g) ! (dR_dg <> g)
	
	e1' = undefined
	e2' = undefined
	--xxzz = h_x*h_x + h_z*h_z
	--e1' = scale (h_z / xxzz) h_x' - scale (h_x / xxzz) h_z'
	--e2' = scale (-sqrt xxzz / (xxzz + h_y*h_y)) h_y' +
	--      scale (h_y / (xxzz + h_y*h_y) / sqrt xxzz) 
	--		(scale h_x h_x' + scale h_z h_z')


-- | Initialize feature from a single measurement. Tuple contains mean theta and phi angles,
-- relative to the robot (the bearing of an observed feature).
-- TODO: implement the angle computations
initialize :: Camera -> Feature -> Landmark
initialize (Camera cpos crot) (Feature landmark_id angles) = Landmark
		landmark_id  (join [cpos, 3 |> [theta, phi, initialRho]])  initialCov where
	h = euler2vec angles
	(theta, phi) = vec2euler . head.toColumns $ crot <> asColumn h

