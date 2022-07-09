module Init where

import System.Console.ANSI

import Data

-- Parameters

texp        = 2      :: Int  -- time rescaling
boostPack   = round (7 * 10 ^ (texp - 1)) :: Boosts
timeStep    = 1 / 10 ^ texp               :: Float
tickDelay   = (5 * 10 ^ (6 - texp))       :: Int
playDelay   = 10 ^ 4 :: Int
gravity     = -1     :: Float
boostForce  = 0.6    :: Float
iniPos      =  7     :: Float
iniVel      = -1     :: Float
altitudeLim = 7.9    :: Float
safeLndVel  = 0.7    :: Float
iniShots    = 15     :: Shots
negShotsPck = -6     :: Shots

-- Configuration (from terminal size)

initConfig :: IO Config
initConfig = do
  Just (sY, sX) <- getTerminalSize
  return $ Config
    {
      lunarX    = div sX 2 - 5,
      statusYX  = (2 * div sY 3, div sX 4 - 8),
      warningYX = (2 * div sY 3, 3 * div sX 4 - 7),
      finalYX   = (5 * div sY 13, div sX 2),
      lastY     = sY
    }

