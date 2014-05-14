
module Parameters where

import Numeric.LinearAlgebra

-- | TODO: tie this with the covariance, defined for observations in Measurement.hs
measurement_cov :: Matrix Double
measurement_cov = diag (2|> [(2*pi / 1600)^^2, (2*pi / 1600)^^2])


-- | TODO: tie this with the covariance, defined for observations in FastSLAM2.hs
initialCov :: Matrix Double
initialCov = diag(6|> [0,0,0,(2*pi / 1600)^^2,(2*pi / 1600)^^2, 1])

initialRho :: Double
initialRho = 0.1

-- | local coords x,y,z movement
kinematicsPosCov = [0.01, 0.01, 0.03]

-- | local coords y-x'-z'' Tait-Bryan angles per sec.
kinematicsRotCov = [0.05, 0.01, 0.01]

-- TODO: number of particles
-- TODO: param prefixes
