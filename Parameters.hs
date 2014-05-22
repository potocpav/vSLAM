{-|
Module      : Parameters
License     : WTFPL
Maintainer  : Pavel Potocek <pavelpotocek@gmail.com>

This package concentrates many parameters of the FastSLAM algorithm in a single
place.

TODO: add param_ prefixes
-}
module Parameters where

import Numeric.LinearAlgebra

numberOfParticles :: Int
numberOfParticles = 20

angular_cov = (2*pi / 1600)^^2 -- a pixel squared

-- | The measurement covariance
measurement_cov :: Matrix Double
measurement_cov = diag (2|> [angular_cov, angular_cov])


-- | The feature initialization covariance. Typically, the two angular components
-- should be identical to 'measurement_cov'.
initialCov :: Matrix Double
initialCov = diag(6|> [0, 0, 0, angular_cov, angular_cov, 1])


-- | The inverse-depth parametrization initial mean. It affects the scale at which
-- the simulation settles.
initialRho :: Double
initialRho = 0.1


-- | The transition covariance diagonal in per-second units. 
kinematicsPosCov = [0.01, 0.01, 0.03]

-- | The rotation covariance diagonal in rad-per-second units.
-- | The coordinates used are the Tait-Bryan y-x'-z" angles per second.
kinematicsRotCov = [0.05, 0.01, 0.01]
--kinematicsRotCov = [0.01 ^^2, 0.01 ^^2, 0.01 ^^2]
