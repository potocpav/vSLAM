
module Simulate (initial, measure, measurement) where

import qualified Data.Set as S

-- Gloss Point definition copy
type Point = (Float,Float)

initial :: IO (S.Set Point)
initial = return $ S.fromList [(1,2),(-3,4),(0,5),(6,7)]

-- TODO: implement
measure :: Point -> Float
measure _ = 5

-- TODO: implement
measurement :: S.Set Point -> S.Set Float
measurement _ = S.fromList [1,2,3]
