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
  drawPlayer $ gs ^. gLeftPlayer
  drawPlayer $ gs ^. gRightPlayer
  drawBall $ gs ^. gBall

  present r

  where drawPlayer = drawRect r . Just . playerToRect
        drawBall = drawRect r . Just . ballToRect
        
