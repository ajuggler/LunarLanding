module Physics where

import Data
import Init

-- Newton's second law

accel :: Boosts -> Float              -- acceleration
accel b = gravity + bForce
  where
    bForce = boostForce * (fromIntegral . boostPacks $ b)

-- Motion

phaseStep :: Boosts -> Phase -> Phase
phaseStep b (Phase p v) = Phase newPos newVel
  where
    newPos = p + timeStep * v         -- position update
    newVel = v + timeStep * (accel b) -- velocity update

-- Number of boosters currently firing

boostPacks :: Boosts -> Int
boostPacks b = div b boostPack + (if mod b boostPack > 0 then 1 else 0)

