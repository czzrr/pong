{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Main where

--import Lib
import SDL
import Foreign.C.Types (CInt)
import Control.Lens
import Control.Monad
import Data.STRef
import Control.Monad.ST

screenDims :: V2 CInt
screenDims = V2 800 600

data GameState = GameState { _gLeftPlayer :: Player,
                             _gRightPlayer :: Player,
                             _gBall :: Ball,
                             _gScore :: Score }

type Pos = (CInt, CInt)
type Vel = (CInt, CInt)

newtype Player = Player { _pPos :: Pos }

playerMovementSpeed :: CInt
playerMovementSpeed = 5

data Ball = Ball { _bPos :: Pos,
                   _bVel :: Vel }

newtype Score = Score { _sScore :: (CInt, CInt) }

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Ball
makeLenses ''Score


playerWidth = 10
playerHeight = 40
playerRect :: Player -> Rectangle CInt
playerRect (Player (x, y)) = Rectangle (P (V2 x y)) (V2 playerWidth playerHeight)

ballSide = 10
ballRect :: Ball -> Rectangle CInt
ballRect (Ball (x, y) _) = Rectangle (P (V2 x y)) (V2 ballSide ballSide)

--white :: V4 CInt
white = V4 255 255 255 255

--black :: V4 CInt
black = V4 0 0 0 255

--framesPerSecond :: Int
framesPerSecond = 60

initialGameState :: GameState
initialGameState = GameState
  { _gLeftPlayer = Player (10, 300),
    _gRightPlayer = Player (780, 300),
    _gBall = Ball (400, 300) (4, -1),
    _gScore = Score (0, 0) }



main :: IO ()
main = do
  initializeAll
  window <- createWindow "Pong" (defaultWindow { windowInitialSize = screenDims })
  renderer <- createRenderer window (-1) defaultRenderer
  
  mainLoop initialGameState renderer

mainLoop :: GameState -> Renderer -> IO ()
mainLoop gs r = do
  drawGameState gs r
  gs' <- updateGameState gs

  delay (1000 `div` framesPerSecond)
  
  hasQuit <- quitIfQPressed
  unless hasQuit (mainLoop gs' r)
  

drawGameState :: GameState -> Renderer -> IO ()
drawGameState gs r = do
  rendererDrawColor r $= black
  clear r
  
  rendererDrawColor r $= white
  drawPlayer leftPlayer
  drawPlayer rightPlayer
  drawBall ball

  if intersectRect (ballToRect ball) (playerToRect leftPlayer) ||
    intersectRect (ballToRect ball) (playerToRect rightPlayer)
    then putStrLn "Collided!"
    else return ()
  present r

  where drawPlayer p = drawRect r (Just $ playerRect p)
        drawBall b = drawRect r (Just $ ballRect b)
        leftPlayer = view gLeftPlayer gs
        rightPlayer = view gRightPlayer gs
        ball = view gBall gs
        
ballToRect :: Ball -> Rectangle CInt
ballToRect b = let (x, y) = view bPos b
                   (dx, dy) = view bVel b
               in Rectangle (P $ V2 x y) (V2 dx dy)

playerToRect :: Player -> Rectangle CInt
playerToRect p = let (x, y) = view pPos p
                 in Rectangle (P $ V2 x y) (V2 playerWidth playerHeight)

                                         
updateGameState :: GameState -> IO GameState
updateGameState gs = do
  keyMap <- getKeyboardState
  let gs' = movePlayers keyMap gs
  let gs'' = moveBall gs'
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
moveBall gs = if intersectRect nbRect (playerToRect leftPlayer) ||
                   intersectRect nbRect (playerToRect rightPlayer)
              then over gBall (incBallPos . reverseXDir) gs
              else set gBall nb gs 
  where nb = incBallPos (view gBall gs)
        nbRect = ballToRect nb
        leftPlayer = view gLeftPlayer gs
        rightPlayer = view gRightPlayer gs
        


incBallPos :: Ball -> Ball
incBallPos b = let (dx, dy) = view bVel b
               in over bPos (\(x, y) -> (x + dx, y + dy)) b

reverseXDir :: Ball -> Ball
reverseXDir = over bVel (\(dx, dy) -> (-dx, dy))

movePlayerUp :: Player -> Player
movePlayerUp = over (pPos . _2) (subtract playerMovementSpeed)

movePlayerDown :: Player -> Player
movePlayerDown = over (pPos . _2) (+ playerMovementSpeed)

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
        
        

eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False

quitIfQPressed :: IO Bool
quitIfQPressed = do
  events <- pollEvents
  let qPressed = any eventIsQPress events
  if qPressed then quit >> return True else return False
  
