
module TestMeasurement where

import Test.QuickCheck hiding ((><))
import Numeric.LinearAlgebra
import Data.Fixed (mod')
import Measurement
import Landmark
import Camera
import InternalMath


a ~= b = abs (a-b) < 10^^(-12) -- ^ equality test for doubles
a ~~= b = (isNaN a && isNaN b) || abs (a-b) < 10^^(-4) -- ^ decreased precision test for doubles
infix 4 ~=, ~~=

-- | Z1,Y2,Z3-type euler angles
euler2matrix (a,b,c) = (3><3)
	[ c1*c2*c3 -s1*s3, -c3*s1 -c1*c2*s3, c1*s2
	, c1*s3 +c2*c3*s1,  c1*c3 -c2*s1*s3, s1*s2
	,          -c3*s2,            s2*s3,    c2 ] where
	c1 = cos a; c2 = cos b; c3 = cos c; s1 = sin a; s2 = sin b; s3 = sin c
	
deepCheck = quickCheckWith stdArgs {maxSuccess = 10000}

--------------------------------------------------------------------------------
-- Real tests follow

-- | vec2euler . euler2vec == id
test_e2v = quickCheck ((\s -> (euler2vec.vec2euler.euler2vec) s ~= euler2vec s) :: (Double, Double) -> Bool)


-- | Testing derivative of measurement equation by derivative definition.
-- This test sometimes fails, but this is hopefully due to precision issues.
-- There are some degenerate configurations, that can cause precision loss.
-- The evidence for this is, that by increasing the error tolerance, the failure
-- rate decreases, in a seemingly smooth fashion.
type Triplet = (Double, Double, Double)
type Hexplet = (Triplet, Triplet)
test_jacobian_l = deepCheck f where
	f :: (Hexplet, Hexplet, Hexplet) -> Bool
	f (((cx,cy,cz),cr),      -- ^random camera
	   ((x,y,z),(theta,phi,rho)),       -- ^random feature
	   ((dx,dy,dz),(dtheta,dphi,drho))) -- ^random derivation direction
	   
		= j1 ~~= j1' && j2 ~~= j2' where
		
		cam = ExactCamera (3|> [cx,cy,cz]) (euler2matrix cr) -- ^ random camera
		feature = 6|> [x,y,z,theta,phi,rho] -- ^ random feature
		-- | random vector of unit length (used as a random direction of a derivative)
		dfeature = let df = 6|> [dx,dy,dz,dtheta,dphi,drho] in
			scale (1/(sqrt.sum.toList $ df*df)) df
		diff = 10^^(-9)
		
		-- | The jacobian being tested
		[j1,j2] = toList $ jacobian_l cam feature <> dfeature
		-- | The jacobian, computed by derivative definition from measurement equation
		[j1',j2'] = (\(a,b) (c,d) -> [(a-c)/diff,(b-d)/diff]) 
		            (measure cam (feature+scale diff dfeature)) (measure cam feature)
			 
-- | Analogous to the test_jacobian_l function.
test_jacobian_c = deepCheck f where
	f :: (Hexplet, Hexplet, Hexplet) -> Bool
	f (((cx,cy,cz),(alpha,beta,gamma)),      -- ^random camera
	   ((x,y,z),(theta,phi,rho)),       -- ^random feature
	   ((dcx,dcy,dcz),(dalpha,dbeta,dgamma))) -- ^random derivation direction
		
		= j1 ~~= j1' && j2 ~~= j2' where
		
		camera = normalize $ 6|> [cx,cy,cz,alpha,beta,gamma]
		feature = 6|> [x,y,z,theta,phi,rho]
		-- | random vector of unit length (used as a random direction of a derivative)
		dcamera = let df = 6|> [dcx,dcy,dcz,dalpha,dbeta,dgamma] in
			scale (1/(sqrt.sum.toList $ df*df)) df
		diff = 10^^(-9)
		
		-- | We need to transform GaussianCamera to an ExactCamera for the measure routine
		ecam c = ExactCamera (3|> [x',y',z']) ((3><3)
			[  sa*sb*sg + ca*cg, -cg*sa*sb + ca*sg, -cb*sa
			,            -cb*sg,             cb*cg,    -sb
			, -ca*sb*sg + cg*sa,  ca*cg*sb + sa*sg,  ca*cb ]) where
				[x',y',z',a',b',g'] = toList c
				sa = sin a'; sb = sin b'; sg = sin g';
				ca = cos a'; cb = cos b'; cg = cos g';
		
		-- | Double-precision rounding errors while summing camera+diff*x made
		-- this routine necessary
		normalize c = 6|> [x',y',z',circ a',circ b',circ g'] where
			[x',y',z',a',b',g'] = toList c
			circ x = mod' x (2*pi)
		
		-- | The values being tested
		[j1,j2] = toList $ jacobian_c (GaussianCamera camera undefined) feature <> dcamera
		-- | The values, computed by derivative definition from measurement equation
		[j1',j2'] = (\(a,b) (c,d) -> [(a-c)/diff,(b-d)/diff]) 
			(measure (ecam $ camera+scale diff dcamera) feature) (measure (ecam camera) feature)
		
-- | Test, if the measurement of the inverse measurement is identity.
-- Inverse measurement is in this context 'landmark initialization'.
test_initialize = quickCheckWith stdArgs {maxSuccess = 10000} f where
	f :: (Hexplet, (Double, Double)) -> Bool
	f (((cx,cy,cz),cr),angles) = a ~= a' && b ~= b' where
		cam = ExactCamera (3|> [cx,cy,cz]) (euler2matrix cr) -- ^ random camera
		(a, b) = normalize angles
		normalize = vec2euler.euler2vec
		
		f_mu = lmu $ initialize cam (Feature undefined angles)
		(a', b') = normalize $ measure cam f_mu
		
--------------------------------------------------------------------------------

test_ea_rot = quickCheckWith stdArgs {maxSuccess = 10000} f where
	f :: Triplet -> Bool
	f (a,b,g) = m `mateq` (euler2rotmat . rotmat2euler $ m) where
		m = euler2rotmat (3|> [a,b,g])
		m1 `mateq` m2 = pnorm PNorm1 (abs (m2-m1)) <= 10^^(-12)

main = do
	test_e2v
	test_jacobian_l
	test_jacobian_c
	test_initialize
	test_ea_rot
