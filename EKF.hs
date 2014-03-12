
-- | Now this is a somehow-well-tested library.

-- TODO: Clean export list. Right now some modules are needed just by the unit test.
module EKF (measure, jacobian, initialize) where

import Numeric.LinearAlgebra
import Feature
import InternalMath


--------------------------------------------------------------------------------
-- Feature initialization parameters

-- | Could be dependent on the position of a measurement sensor.
-- | TODO: tie this with the covariance, defined for observations in PHDSLAM.hs
initialCov :: Matrix Double
initialCov = diag(6|> [0,0,0,0.003,0.003,0.5])

-- | TODO: check the correctness of this value (10 or 1/10 or other?)
initialRho = 0.2

-- | The eta-value of newly-born features. It could "be arbitrarily small".
initialEta = 0.01

--------------------------------------------------------------------------------


-- | 6D Feature mean to a directional (un-normalized) vector
measure_h :: Camera -> Vector Double -> Vector Double
measure_h (Camera cp cr) f = 
		head.toColumns $ trans cr <> asColumn (scale rho (fpos - cp) + euler2vec (theta, phi)) where
	[fpos, tmp] = takesV [3,3] f
	[theta, phi, rho] = toList tmp


-- | The projection of the 6D Feature mean into theta-phi parametrisation.
measure :: Camera -> Vector Double -> (Double, Double)
measure c f = vec2euler $ measure_h c f


-- | Measurement equation jacobian
jacobian :: Camera -> Vector Double -> Matrix Double
jacobian (cam@(Camera cp cr)) f = fromRows [e1', e2'] where
	[x, y, z, theta, phi, rho] = toList f
	[x_c, y_c, z_c] = toList cp
	[h_x, h_y, h_z] = toList $ measure_h cam f
	h' = trans cr <> ((3><6) 
		[ rho,   0,   0,  cos theta * cos phi, -sin theta * sin phi, x - x_c
		,   0, rho,   0,                    0,             -cos phi, y - y_c
		,   0,   0, rho, -sin theta * cos phi, -cos theta * sin phi, z - z_c ])
	[h_x', h_y', h_z'] = toRows h'
	
	xxzz = h_x*h_x + h_z*h_z
	e1' = scale (h_z / xxzz) h_x' - scale (h_x / xxzz) h_z'
	e2' = scale (-sqrt xxzz / (xxzz + h_y*h_y)) h_y' +
	      scale (h_y / (xxzz + h_y*h_y) / sqrt xxzz) 
			(scale h_x h_x' + scale h_z h_z')



-- | Initialize feature from a single measurement. Tuple contains mean theta and phi angles,
-- relative to the robot (the bearing of an observed feature).
-- TODO: implement the angle computations
initialize :: Camera -> Measurement -> Feature
initialize (Camera cpos crot) angles = Feature 
		initialEta  (join [cpos, 3 |> [theta, phi, initialRho]])  initialCov where
	h = euler2vec angles
	(theta, phi) = vec2euler . head.toColumns $ crot <> asColumn h

