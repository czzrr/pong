{-# LANGUAGE TemplateHaskell #-}

module Game where

import SDL.Input.Keyboard
import SDL.Vect
import SDL.Video.Renderer
import Foreign.C.Types (CInt)
import Control.Lens

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

playerRect :: Player -> Rectangle CInt
playerRect (Player (x, y)) = Rectangle (P (V2 x y)) (V2 playerWidth playerHeight)

ballSide :: CInt
ballSide = 10

ballRect :: Ball -> Rectangle CInt
ballRect (Ball (x, y) _) = Rectangle (P (V2 x y)) (V2 ballSide ballSide)

ballToRect :: Ball -> Rectangle CInt
ballToRect b = let (x, y) = view bPos b
                   (dx, dy) = view bVel b
               in Rectangle (P $ V2 x y) (V2 dx dy)

playerToRect :: Player -> Rectangle CInt
playerToRect p = let (x, y) = view pPos p
                 in Rectangle (P $ V2 x y) (V2 playerWidth playerHeight)

intersectRect :: Rectangle CInt -> Rectangle CInt -> Bool
intersectRect (Rectangle (P (V2 x y)) (V2 dx dy))
              (Rectangle (P (V2 x' y')) (V2 dx' dy')) =
  xCol && yCol
  where xCol = (left >= left' && left <= right') && (right >= left' || right <= right')
        yCol = (top >= top' && top <= bottom') && (bottom >= top' && bottom <= bottom')
        top = y
        top' = y'
        bottom = y + dy
        bottom' = y' + dy'
        left = x
        left' = x'
        right = x + dx
        right' = x' + dx'


screenDims :: V2 CInt
screenDims = V2 800 600

playerWidth :: CInt
playerWidth = 10

playerHeight :: CInt
playerHeight = 40




 
initialBall :: Ball
initialBall = Ball (400, 300) (5, -3)

initialGameState :: GameState
initialGameState = GameState
  { _gLeftPlayer = Player (10, 300),
    _gRightPlayer = Player (780, 300),
    _gBall = initialBall,
    _gScore = Score (0, 0) }



playerMovementSpeed :: CInt
playerMovementSpeed = 8

updateGameState :: GameState -> IO GameState
updateGameState gs = do
  keyMap <- getKeyboardState
  let gs'  = movePlayers keyMap gs
      gs'' = moveBall gs'
  return gs''

movePlayers :: (Scancode -> Bool) -> GameState -> GameState
movePlayers keyMap gs = moveRightPlayer keyMap (moveLeftPlayer keyMap gs)

moveLeftPlayer :: (Scancode -> Bool) -> GameState -> GameState
moveLeftPlayer keyMap gs
  | keyMap ScancodeW = over gLeftPlayer movePlayerUp gs 
  | keyMap ScancodeS = over gLeftPlayer movePlayerDown gs
  | otherwise = gs

moveRightPlayer :: (Scancode -> Bool) -> GameState -> GameState
moveRightPlayer keyMap gs
  | keyMap ScancodeUp = over gRightPlayer movePlayerUp gs
  | keyMap ScancodeDown = over gRightPlayer movePlayerDown gs
  | otherwise = gs

moveBall :: GameState -> GameState
moveBall gs | leftPlayerScored         = set gBall initialBall $
                                           over gScore
                                           (\(Score (ls, rs)) -> Score (ls + 1, rs)) gs
            | rightPlayerScored        = over gBall reverseXDir $ set gBall initialBall $
                                           over gScore
                                           (\(Score (ls, rs)) -> Score (ls, rs + 1)) gs
            | ballIntersectsWithPlayer = over gBall (incBallPos . reverseXDir) gs
            | ballHitsTopOrBottom      = over gBall (incBallPos . reverseYDir) gs
            | otherwise                = set gBall nb gs 
  where ball = view gBall gs
        ballIntersectsWithPlayer = intersectRect nbRect (playerToRect leftPlayer) ||
          intersectRect nbRect (playerToRect rightPlayer)
        nb = incBallPos (view gBall gs)
        nbRect = ballToRect nb
        leftPlayer = view gLeftPlayer gs
        rightPlayer = view gRightPlayer gs
        leftPlayerScored = let x = view (bPos . _1) ball in x > view _x screenDims
        rightPlayerScored = let x = view (bPos . _1) ball in x < 0
        ballHitsTopOrBottom = let y = view (bPos . _2) ball in
                                y > view _y screenDims || y < 0


incBallPos :: Ball -> Ball
incBallPos b = let (dx, dy) = view bVel b
               in over bPos (\(x, y) -> (x + dx, y + dy)) b

reverseXDir :: Ball -> Ball
reverseXDir = over (bVel . _1) negate

reverseYDir :: Ball -> Ball
reverseYDir = over (bVel . _2) negate

movePlayerUp :: Player -> Player
movePlayerUp = over (pPos . _2) (subtract playerMovementSpeed)

movePlayerDown :: Player -> Player
movePlayerDown = over (pPos . _2) (+ playerMovementSpeed)

