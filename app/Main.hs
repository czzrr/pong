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

data Ball = Ball { _bPos :: Pos,
                      _bVel :: Vel }

newtype Score = Score { _sScore :: (CInt, CInt) }

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Ball
makeLenses ''Score

playerMovementSpeed = 5

initialGameState :: GameState
initialGameState = GameState
  { _gLeftPlayer = Player (10, 300),
    _gRightPlayer = Player (780, 300),
    _gBall = Ball (400, 300) (2, 0),
    _gScore = Score (0, 0) }
 
main :: IO ()
main = do
  initializeAll
  window <- createWindow "Pong" (defaultWindow { windowInitialSize = screenDims })
  renderer <- createRenderer window (-1) defaultRenderer
  
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 255 255 255

  mainLoop initialGameState renderer

mainLoop :: GameState -> Renderer -> IO ()
mainLoop gs r = do
  drawGameState gs r
  gs' <- updateGameState gs

  delay (1000 `div` 60)
  
  hasQuit <- quitIfQPressed
  unless hasQuit (mainLoop gs' r)
  

drawGameState :: GameState -> Renderer -> IO ()
drawGameState gs r = do
  rendererDrawColor r $= V4 0 0 0 255
  clear r
  rendererDrawColor r $= V4 255 255 255 255
  
  drawPlayer (view gLeftPlayer gs)
  drawPlayer (view gRightPlayer gs)
  drawBall (view gBall gs)
  present r

  where drawPlayer p = drawRect r (Just $ playerRect p)
        drawBall b = drawRect r (Just $ ballRect b)

playerRect :: Player -> Rectangle CInt
playerRect (Player (x, y)) = Rectangle (P (V2 x y)) (V2 10 40)

ballRect :: Ball -> Rectangle CInt
ballRect (Ball (x, y) _) = Rectangle (P (V2 x y)) (V2 10 10)

updateGameState :: GameState -> IO GameState
updateGameState gs = do
  kbState <- getKeyboardState
  let gs' = movePlayers kbState gs
  let gs'' = moveBall gs'
  return gs''

movePlayers :: (Scancode -> Bool) -> GameState -> GameState
movePlayers f gs = moveRightPlayer f (moveLeftPlayer f gs)

moveLeftPlayer f gs
  | f ScancodeW = over gLeftPlayer movePlayerUp gs 
  | f ScancodeS = over gLeftPlayer movePlayerDown gs
  | otherwise = gs

moveRightPlayer f gs
  | f ScancodeUp = over gRightPlayer movePlayerUp gs
  | f ScancodeDown = over gRightPlayer movePlayerDown gs
  | otherwise = gs

moveBall :: GameState -> GameState
moveBall gs = over gBall incBallPos gs

incBallPos :: Ball -> Ball
incBallPos b = let v@(dx, dy) = view bVel b
               in over bPos (\(x, y) -> (x + dx, y + dy)) b
  

movePlayerUp :: Player -> Player
movePlayerUp = over (pPos . _2) (subtract playerMovementSpeed)

movePlayerDown :: Player -> Player
movePlayerDown = over (pPos . _2) (+ playerMovementSpeed)



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
  
