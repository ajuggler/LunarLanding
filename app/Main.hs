module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import System.IO
import System.Console.ANSI

import Data
import Init
import Render

-- Threads

tick :: Chan Event -> IO ()
tick chan = forever $ do
  threadDelay tickDelay
  writeChan chan TickEvent

input :: Chan Event -> IO ()
input chan = forever $ do
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  writeChan chan (KeyEvent c)

-- Play

play :: Chan Event -> StateT Status (ReaderT Config IO) ()
play chan = do
  (b, s, ph) <- get
  let (Phase p v) = ph
  case (p <= 0, abs v < safeLndVel) of
    (True, True)  -> lift $ finalDraw Win s ph
    (True, False) -> lift $ finalDraw LoseCrash s ph
    (False, _)    -> if p > altitudeLim
                then lift $ finalDraw LoseEscape s ph
                else continuePlay chan

continuePlay :: Chan Event -> StateT Status (ReaderT Config IO) ()
continuePlay chan = do
  (b, s, ph) <- get
  lift $ statusDraw b s ph
  liftIO . threadDelay $ playDelay
  let newPhase = phaseStep b ph
  c <- liftIO . readChan $ chan
  case c of
    TickEvent    -> do
      let newBoost = if b > 0 then b - boostPacks b else 0
      let newShots = if s < 0 then s + 1 else s
      put $ (newBoost, newShots, newPhase)
      play chan
    KeyEvent ' ' -> if s > 0
      then do
        let newBoost = b + boostPack
        let newShots = s - 1
        put (newBoost, newShots, newPhase)
        play chan
      else do
        put (b, negShotsPck, ph)
        play chan
    KeyEvent 'x' -> lift byeMsg
    KeyEvent _   -> play chan

-- Physics

-- Newton's second law:
accel :: Boosts -> Float              -- acceleration
accel b = gravity + bForce
  where
    bForce = boostForce * (fromIntegral . boostPacks $ b)

-- Motion:
phaseStep :: Boosts -> Phase -> Phase
phaseStep b (Phase p v) = Phase newPos newVel
  where
    newPos = p + timeStep * v         -- position update
    newVel = v + timeStep * (accel b) -- velocity update

-- Main

main :: IO ()
main = do
  config <- initConfig
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  chan <- newChan
  forkIO $ tick chan
  forkIO $ input chan
  let iniStatus = (0, iniShots, Phase iniPos iniVel)
  runReaderT (runStateT (play chan) iniStatus) config
  showCursor
  


