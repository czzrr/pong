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
playerWidth = 12

playerHeight :: CInt
playerHeight = 60

playerMovementSpeed :: CInt
playerMovementSpeed = 8

ballSide :: CInt
ballSide = 15

initBallSpeed :: Double
initBallSpeed = 9.0

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
  gs' <- fmap (handleScore . moveBall) . handleInput $ gs
  return gs'

moveBall :: GameState -> GameState
moveBall gs | ballHitsLeftPlayer  = if tooLate
                                    then gs & gBall %~ (incBallPos . reverseYDir)
                                    else set gBall (bounce b lp) gs
            | ballHitsRightPlayer = if tooLate
                                    then gs & gBall %~ (incBallPos . reverseYDir)
                                    else set gBall (bounce b rp) gs
            | ballHitsTopOrBottom = gs & gBall %~ (incBallPos . reverseYDir)
            | otherwise           = gs & gBall %~ incBallPos
  where ballHitsLeftPlayer  = intersectRect nbr (playerToRect lp)
        ballHitsRightPlayer = intersectRect nbr (playerToRect rp)
        b                   = gs ^. gBall
        nb                  = incBallPos b
        lp                  = gs ^. gLeftPlayer
        rp                  = gs ^. gRightPlayer
        nbr                 = ballToRect nb
        ballHitsTopOrBottom = let y = b ^. bPos . _2
                              in y > screenHeight - ballSide || y < 0
        tooLate             = let r = b ^. bPos ^. _1 + ballSide
                                  l = r - ballSide
                              in l < lp ^. pPos ^. _1 || r > rp ^. pPos ^. _1
                                 
handleScore :: GameState -> GameState
handleScore gs
  | leftPlayerScored  = resetBall . incLPScore $ gs
  | rightPlayerScored = resetBall . incRPScore $ gs
  | otherwise         = gs
  where leftPlayerScored    = x > screenWidth + 20
        rightPlayerScored   = x < (-40)
        x = gs ^. gBall . bPos . _1
        incLPScore = (gScore . sScore . _1) %~ (+1)
        incRPScore = (gScore . sScore . _2) %~ (+1)

resetBall :: GameState -> GameState
resetBall = gBall .~ initialBall

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

bounce :: Ball -> Player -> Ball
bounce b p | r <= 0.1   = adjust (-8)
           | r <= 0.2   = adjust (-6)
           | r <= 0.3   = adjust (-4)
           | r <= 0.4   = adjust (-2)
           | r <= 0.6   = adjust 0
           | r <= 0.7   = adjust 2
           | r <= 0.8   = adjust 4
           | r <= 0.9   = adjust 6
           | otherwise  = adjust 8
           where r            = u / (fromIntegral (ballSide + playerHeight) :: Double)
                 u            = fromIntegral $ b ^. bPos . _2 + ballSide - p ^. pPos ^. _2
                 adjust dy    = incBallPos . reverseXDir $
                                 (bVel . _1) .~ (getNewDX dy) $
                                 (bVel . _2) .~ dy $ b
                 getNewDX dy  = (signum $ b ^. bVel . _1) *
                                 (ceiling . sqrt $ initBallSpeed^2 - (fromIntegral dy)^2)


