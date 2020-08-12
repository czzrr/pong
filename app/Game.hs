{-# LANGUAGE TemplateHaskell #-}

module Game where

import SDL.Input.Keyboard
import SDL.Vect
import SDL.Video.Renderer
import Foreign.C.Types (CInt)
import Control.Lens

screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600

screenDims :: V2 CInt
screenDims = V2 screenWidth screenHeight

data GameState = GameState { _gLeftPlayer :: Player,
                             _gRightPlayer :: Player,
                             _gBall :: Ball,
                             _gScore :: Score }

type Pos = (CInt, CInt)
type Vel = (CInt, CInt)

newtype Player = Player { _pPos :: Pos }

data Ball = Ball { _bPos :: Pos,
                   _bVel :: Vel }

newtype Score = Score { _sScore :: (CInt, CInt) }

instance Show Score where
  show (Score (l, r)) = "(" ++ show l ++ ", " ++ show r ++ ")"
  
makeLenses ''GameState
makeLenses ''Player
makeLenses ''Ball
makeLenses ''Score

playerWidth :: CInt
playerWidth = 10

playerHeight :: CInt
playerHeight = 50

playerMovementSpeed :: CInt
playerMovementSpeed = 8

ballSide :: CInt
ballSide = 15

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
  { _gLeftPlayer = Player (0, screenWidth `div` 2),
    _gRightPlayer = Player (screenWidth - playerWidth, screenWidth `div` 2),
    _gBall = initialBall,
    _gScore = Score (0, 0) }

updateGameState :: GameState -> IO GameState
updateGameState gs = do
  gs' <- fmap (gBall %~ moveBall) . handleInput $ gs
  let ball = moveBall (gs'' ^. gBall)
  checkScore
  -- if scored then reset ball and update score else moveBall
  let score = checkScore (gs ^. gScore)
  
  let ball = moveBall (gs'' ^. gBall)

  return gs''
  where checkScore s | leftPlayerScored = Just $ incLPScore s
                     | rightPlayerScored = Just $ incRPScore s
                     | otherwise = Nothing
        where x = ball ^. bPos . _1
              leftPlayerScored    = x > screenDims ^. _x
              rightPlayerScored   = x < 0

        moveBall b | ballHitsPlayer = incBallPos . reverseXDir $ b
                   | ballHitsTopOrBottom = incBallPos . reverseYDir $ b
                   | otherwise = b
        where ballHitsPlayer      = intersectRect br (playerToRect $ gs ^. gLeftPlayer) ||
                                      intersectRect br (playerToRect $ gs ^. gRightPlayer)
              br = ballToRect b
              ballHitsTopOrBottom = let y = ball ^. bPos . _2
                              in y > screenDims ^. _y - ballSide || y < 0

handleInput :: GameState -> IO GameState
handleInput gs = do
  keyMap <- getKeyboardState
  return $ gLeftPlayer %~ (handleLeftPlayer keyMap) $
    gRightPlayer %~ (handleRightPlayer keyMap) $ gs
  
handleLeftPlayer :: (Scancode -> Bool) -> Player -> Player
handleLeftPlayer keyMap p
  | keyMap ScancodeW = movePlayerUp p
  | keyMap ScancodeS = movePlayerDown p
  | otherwise = p

handleRightPlayer :: (Scancode -> Bool) -> Player -> Player
handleRightPlayer keyMap p
  | keyMap ScancodeUp = movePlayerUp p
  | keyMap ScancodeDown = movePlayerDown p
  | otherwise = p

incBallPos :: Ball -> Ball
incBallPos b = let (dx, dy) = b ^. bVel
               in bPos %~ (\(x, y) -> (x + dx, y + dy)) $ b

moveBall :: Ball -> Ball
moveBall b | ballHitsPlayer = 


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
        x                   = ball ^. bPos . _1
        leftPlayerScored    = x > screenDims ^. _x
        rightPlayerScored   = x < 0
        ballHitsTopOrBottom = let y = ball ^. bPos . _2
                              in y > screenDims ^. _y - ballSide || y < 0

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



reverseXDir :: Ball -> Ball
reverseXDir = (bVel . _1) %~ negate

reverseYDir :: Ball -> Ball
reverseYDir = (bVel . _2) %~ negate



