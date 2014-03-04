
module TestEKF3D where

import Test.QuickCheck hiding ((><))
import Numeric.LinearAlgebra
import EKF3D
import Feature3D
import InternalMath

-- for debugging
import System.IO.Unsafe (unsafePerformIO)
debug :: Show a => String -> a -> a
debug s a = unsafePerformIO (putStrLn $ s ++ ": " ++ show a) `seq` a
infixr 1 `debug`


a ~= b = abs (a-b) < 10^^(-12) -- ^ equality test for doubles
a ~~= b = (isNaN a && isNaN b) || abs (a-b) < 10^^(-4) -- ^ decreased precision test for doubles
infix 4 ~=, ~~=

-- | Z1,Y2,Z3-type euler angles
euler2matrix (a,b,c) = (3><3)
	[ c1*c2*c3 -s1*s3, -c3*s1 -c1*c2*s3, c1*s2
	, c1*s3 +c2*c3*s1,  c1*c3 -c2*s1*s3, s1*s2
	,          -c3*s2,            s2*s3,    c2 ] where
	c1 = cos a; c2 = cos b; c3 = cos c; s1 = sin a; s2 = sin b; s3 = sin c

--------------------------------------------------------------------------------
-- Real tests follow

-- | vec2euler . euler2vec == id
test_e2v = quickCheck ((\s -> (euler2vec.vec2euler.euler2vec) s ~= euler2vec s) :: (Double, Double) -> Bool)


-- | Testing derivative of measurement equation by derivative definition.
-- This test rarely fails, but this is probably due to precision issues.
-- There are some degenerate configurations, that can cause precision loss.
-- The evidence for this is, that by increasing the error tolerance, the failure
-- rate decreases, in a seemingly smooth fashion.
type Triplet = (Double, Double, Double)
type Hexplet = (Triplet, Triplet)
test_jacobian = quickCheckWith stdArgs {maxSuccess = 10000} f where
	f :: (Hexplet, Hexplet, Hexplet) -> Bool
	f (((cx,cy,cz),cr),      -- ^random camera
	   ((x,y,z),(theta,phi,rho)),       -- ^random feature
	   ((dx,dy,dz),(dtheta,dphi,drho))) -- ^random derivation direction
	   
		= j1 ~~= j1' && j2 ~~= j2' where
		
		cam = Camera (3|> [cx,cy,cz]) (euler2matrix cr) -- ^ random camera
		feature = 6|> [x,y,z,theta,phi,rho] -- ^ random feature
		-- | random vector of unit length (used as a random direction of a derivative)
		dfeature = let df = 6|> [dx,dy,dz,dtheta,dphi,drho] in
			 scale (1/(sqrt.sum.toList $ df*df)) df
		diff = 10^^(-9)
		
		-- | The jacobian being tested
		[j1,j2] = toList $ jacobian cam feature <> dfeature
		-- | The jacobian, computed by derivative definition from measurement equation
		[j1',j2'] = (\(a,b) (c,d) -> [(a-c)/diff,(b-d)/diff]) 
		            (measure cam (feature+scale diff dfeature)) (measure cam feature)
			 
		
-- | Test, if the measurement of the inverse measurement is identity.
-- Inverse measurement is in this context 'landmark initialization'.
test_initialize = quickCheckWith stdArgs {maxSuccess = 10000} f where
	f :: (Hexplet, (Double, Double)) -> Bool
	f (((cx,cy,cz),cr),angles) = a ~= a' && b ~= b' where
		cam = Camera (3|> [cx,cy,cz]) (euler2matrix cr) -- ^ random camera
		(a, b) = normalize angles
		normalize = vec2euler.euler2vec
		
		f_mu = mu $ initialize cam angles
		(a', b') = normalize $ measure cam f_mu
		
main = do
	test_e2v
	test_jacobian
	test_initialize
