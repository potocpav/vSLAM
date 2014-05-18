
module Parameters where

import Numeric.LinearAlgebra

angular_cov = (2*pi / 1600)^^2 -- a pixel squared

-- | TODO: tie this with the covariance, defined for observations in Measurement.hs
measurement_cov :: Matrix Double
measurement_cov = diag (2|> [angular_cov, angular_cov])


-- | TODO: tie this with the covariance, defined for observations in FastSLAM2.hs
initialCov :: Matrix Double
initialCov = diag(6|> [0, 0, 0, angular_cov, angular_cov, 1])

initialRho :: Double
initialRho = 0.1

-- | local coords x,y,z movement
kinematicsPosCov = [0.01, 0.01, 0.03]

-- | local coords y-x'-z'' Tait-Bryan angles per sec.
kinematicsRotCov = [0.05, 0.01, 0.01]
--kinematicsRotCov = [0.01 ^^2, 0.01 ^^2, 0.01 ^^2]

-- TODO: number of particles
-- TODO: param prefixes
