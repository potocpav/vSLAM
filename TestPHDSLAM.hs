
import Numeric.LinearAlgebra

import Graphics.Plot
import PHDSLAM

plot_normalDensity =
	mesh $ build (round dx,round dy) (\i j -> normalDensity
			(2|> [0,0])
			((2><2)[0.1,0,0,1])
			(2|>[i/dx*sx*2 - sx + cx,j/dy*sy*2 - sy + cy])) where
		(cx,cy) = (0,0)   -- center
		(sx,sy) = (3,3)   -- side
		(dx,dy) = (50,50) -- detail

