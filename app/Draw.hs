{-# LANGUAGE OverloadedStrings #-}

module Draw where

import SDL
import SDL.Font
import Foreign.C.Types (CInt)
import Game
import Control.Lens
import Data.Word (Word8, Word32)
import Data.Text (pack)

white :: V4 Word8
white = V4 255 255 255 255

black :: V4 Word8
black = V4 0 0 0 255

--scoreFont = load "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" 32

scoreRect = Rectangle
              (P $ V2 (screenWidth `div` 2 - 40) 0)
              (V2 80 50)

drawGameState :: GameState -> Renderer -> Font -> IO ()
drawGameState gs r sf = do
  rendererDrawColor r $= black
  clear r
  
  rendererDrawColor r $= white
  drawPlayer $ gs ^. gLeftPlayer
  drawPlayer $ gs ^. gRightPlayer
  drawBall $ gs ^. gBall

  scoreSurface <- blended sf white (pack . show $ gs ^. gScore)
  scoreTexture <- createTextureFromSurface r scoreSurface
  copy r scoreTexture Nothing (Just scoreRect)
  
  present r

  where drawPlayer = drawRect r . Just . playerToRect
        drawBall = drawRect r . Just . ballToRect
        
