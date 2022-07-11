module Render where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI

import Data
import Init
import Physics

-- Messages

byeMsg :: ReaderT Config IO ()
byeMsg = do
  config <- ask
  lift $ setCursorPosition (lastY config - 1) 0
  lift . putStrLn $ "Bye!"

finalMsg :: GameResult -> ReaderT Config IO ()
finalMsg gr = do
  config <- ask
  let (y0, x0) = finalYX config
  let msg = case gr of
        Win        -> "Safe Moonlanding! You Win!"
        LoseCrash  -> "Crashed! You Lose!"
        LoseEscape -> "Lost in Space! You Lose!"
  let dx = div (length msg - 1) 2
  lift $ setCursorPosition y0 (x0 - dx)
  lift . putStr $ msg

phaseMsg :: Pos -> Vel -> ReaderT Config IO ()
phaseMsg p v = do
  config <- ask
  lift . (uncurry setCursorPosition) $ statusYX config
  lift . putStr $ show (Phase p v)
  if -v > safeLndVel
    then do
      lift . (uncurry setCursorPosition) $ warningYX config
      lift . putStr $ "Too Fast for Landing!"
    else return ()

shotsMsg :: Shots -> ReaderT Config IO ()
shotsMsg s = do
  config <- ask
  let (y0, x0) = statusYX config
  lift $ setCursorPosition (y0 + 2) x0
  if s >= 0
    then lift . putStr $ "Shots left: " ++ show s
    else lift . putStr $ "Shots left: 0  Damn it!"

fireMsg :: Int -> ReaderT Config IO ()
fireMsg n = do
  config <- ask
  let (y0, x0) = statusYX config
  lift $ setCursorPosition (y0 - n) x0
  lift . putStr $ "Fired!"

-- Rendering

lunarModuleDraw :: Boosts -> Pos -> ReaderT Config IO ()
lunarModuleDraw b p = do
  config <- ask
  let y0 = fromIntegral . lastY $ config
  let x1 = lunarX config
  let y1 = round $ y0 * (1 - p / iniPos)
  if y1 < 0
    then return ()
    else do
      lift $ setCursorPosition y1 x1
      lift . putStr $ "¦--¤-¤-¤--¦"
  if b <= 0 || y1 < -1 || y1 > lastY config - 2
    then return ()
    else do
      lift $ setCursorPosition (y1 + 1) (x1 + 3)
      lift . setSGR $ [SetColor Foreground Vivid Red]
      lift . putStr $ "| | |"
      lift . setSGR $ [Reset]

lunarModuleCrashDraw :: ReaderT Config IO ()
lunarModuleCrashDraw = do
  config <- ask
  let y1 = fromIntegral . lastY $ config
  let x1 = lunarX config
  lift $ setCursorPosition y1 x1
  lift . putStr $ "¦--/\\×/\\--¦"

statusDraw :: Boosts -> Shots -> Phase -> ReaderT Config IO ()
statusDraw b s (Phase p v) = do
  config <- ask
  lift clearScreen
  phaseMsg p v
  shotsMsg s
  foldr (\n r -> fireMsg n >> r) (return ()) [1..boostPacks b]
  lunarModuleDraw b p
  lift $ setCursorPosition (lastY config) 0
  
finalDraw :: GameResult -> Shots -> Phase -> ReaderT Config IO ()
finalDraw gr s (Phase p v) = do
  config <- ask
  let s' = if s >= 0 then s else 0
  lift clearScreen
  case gr of
    Win -> phaseMsg 0 v >> shotsMsg s' >> lunarModuleDraw 0 0
    LoseCrash -> phaseMsg 0 v >> shotsMsg s' >> lunarModuleCrashDraw
    LoseEscape -> phaseMsg p v >> shotsMsg s'
  finalMsg gr
  lift $ setCursorPosition (lastY config) 0

