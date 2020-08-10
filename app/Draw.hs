module Draw where

import SDL
import Foreign.C.Types (CInt)
import Game
import Control.Lens
import Data.Word (Word8, Word32)



white :: V4 Word8
white = V4 255 255 255 255

black :: V4 Word8
black = V4 0 0 0 255

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
        
