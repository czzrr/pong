{-# LANGUAGE TemplateHaskell #-}

module Game where

import SDL.Input.Keyboard
import SDL.Vect
import SDL.Video.Renderer
import Foreign.C.Types (CInt)
import Control.Lens

screenDims :: V2 CInt
screenDims = V2 800 600

data GameState = GameState { _gLeftPlayer :: Player,
                             _gRightPlayer :: Player,
                             _gBall :: Ball,
                             _gScore :: Score }

type Pos = (CInt, CInt)
type Vel = (CInt, CInt)

newtype Player = Player { _pPos :: Pos }

data Ball = Ball { _bPos :: Pos,
                   _bVel :: Vel }

newtype Score = Score { _sScore :: (CInt, CInt) } deriving Show

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Ball
makeLenses ''Score

playerWidth :: CInt
playerWidth = 10

playerHeight :: CInt
playerHeight = 40

playerMovementSpeed :: CInt
playerMovementSpeed = 8

ballSide :: CInt
ballSide = 10

playerToRect :: Player -> Rectangle CInt
playerToRect p = let (x, y) = p ^. pPos
                 in Rectangle (P $ V2 x y) $ V2 playerWidth playerHeight

ballToRect :: Ball -> Rectangle CInt
ballToRect b = let (x, y) = b ^. bPos
               in  Rectangle (P $ V2 x y) $ V2 ballSide ballSide

intersectRect :: Rectangle CInt -> Rectangle CInt -> Bool
intersectRect (Rectangle (P (V2 x  y))  (V2 dx  dy))
              (Rectangle (P (V2 x' y')) (V2 dx' dy')) =
  xCol && yCol
  where xCol = (left >= left' && left <= right') || (right >= left' && right <= right')
        yCol = (top >= top' && top <= bottom') || (bottom >= top' && bottom <= bottom')
        (top, bottom, left, right)     = (y, y + dy, x, x + dx)
        (top', bottom', left', right') = (y', y' + dy', x', x' + dx')

initialBall :: Ball
initialBall = Ball (400, 300) (5, -3)

initialGameState :: GameState
initialGameState = GameState
  { _gLeftPlayer = Player (0, screenDims ^. _x `div` 2),
    _gRightPlayer = Player (screenDims ^. _x - playerWidth, screenDims ^. _x `div` 2),
    _gBall = initialBall,
    _gScore = Score (0, 0) }



updateGameState :: GameState -> IO GameState
updateGameState gs = do
  keyMap <- getKeyboardState
  let gs'  = movePlayers keyMap gs
      gs'' = moveBall gs'
  return gs''

movePlayers :: (Scancode -> Bool) -> GameState -> GameState
movePlayers keyMap gs = moveRightPlayer keyMap $ moveLeftPlayer keyMap gs

moveLeftPlayer :: (Scancode -> Bool) -> GameState -> GameState
moveLeftPlayer keyMap gs
  | keyMap ScancodeW = gLeftPlayer %~ movePlayerUp $ gs
  | keyMap ScancodeS = gLeftPlayer %~ movePlayerDown $ gs
  | otherwise = gs

moveRightPlayer :: (Scancode -> Bool) -> GameState -> GameState
moveRightPlayer keyMap gs
  | keyMap ScancodeUp = gRightPlayer %~ movePlayerUp $ gs
  | keyMap ScancodeDown = gRightPlayer %~ movePlayerDown $ gs
  | otherwise = gs

moveBall :: GameState -> GameState
moveBall gs | leftPlayerScored         = gBall .~ initialBall $ gScore %~ incLPScore $ gs
            | rightPlayerScored        = gBall %~ reverseXDir $
                                           gBall .~ initialBall $
                                           gScore %~ incRPScore $
                                           gs
            | ballHitsPlayer           = gBall %~ (incBallPos . reverseXDir) $ gs
            | ballHitsTopOrBottom      = gBall %~ (incBallPos . reverseYDir) $ gs
            | otherwise                = gBall .~ nb $ gs
  where ball                = view gBall gs
        br = ballToRect ball
        ballHitsPlayer      = intersectRect br (playerToRect $ gs ^. gLeftPlayer) ||
                                intersectRect br (playerToRect $ gs ^. gRightPlayer)
        nb                  = incBallPos $ gs ^. gBall
        nbRect              = ballToRect nb
        leftPlayerScored    = let x = ball ^. bPos . _1 in x > screenDims ^. _x
        rightPlayerScored   = let x = ball ^. bPos . _1 in x < 0
        ballHitsTopOrBottom = let y = ball ^. bPos . _2 in
                                y > screenDims ^. _y - ballSide || y < 0

incLPScore, incRPScore :: Score -> Score
incLPScore = (sScore . _1) %~ (+1)
incRPScore = (sScore . _2) %~ (+1)

movePlayerUp :: Player -> Player
movePlayerUp p = (pPos . _2) %~ f $ p
               where f y = if y - playerMovementSpeed < 0
                           then y
                           else y - playerMovementSpeed

movePlayerDown :: Player -> Player
movePlayerDown p = (pPos . _2) %~ f $ p
               where f y = if y + playerMovementSpeed + playerHeight > screenDims ^. _2
                           then y
                           else y + playerMovementSpeed

incBallPos :: Ball -> Ball
incBallPos b = let (dx, dy) = b ^. bVel
               in bPos %~ (\(x, y) -> (x + dx, y + dy)) $ b

reverseXDir :: Ball -> Ball
reverseXDir = (bVel . _1) %~ negate

reverseYDir :: Ball -> Ball
reverseYDir = (bVel . _2) %~ negate



