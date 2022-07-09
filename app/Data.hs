module Data where

type Boosts = Int    -- units of booster force
type Shots  = Int    -- booster shots left (if positive)
type Pos    = Float  -- position
type Vel    = Float  -- velocity

data Phase = Phase Pos Vel

type Status = (Boosts, Shots, Phase)

instance Show Phase where
  show (Phase p v) = "pos: " ++ show p' ++ ", vel: " ++ show v'
    where
      toDgts d x = round $ d * x
      [p', v'] = map (toDgts 100) [p, v]

data Event = TickEvent | KeyEvent Char

data GameResult = Win | LoseCrash | LoseEscape

-- Config gives position anchors needed for rendering

data Config = Config
  {
    lunarX    :: Int,          -- lunar module
    statusYX  :: (Int, Int),   -- phase status
    warningYX :: (Int, Int),   -- velocity warning
    finalYX   :: (Int, Int),   -- final message
    lastY     :: Int           -- last row
  }


