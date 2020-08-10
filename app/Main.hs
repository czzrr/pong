{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Main where

import SDL
import Foreign.C.Types (CInt)
import Control.Lens
import Control.Monad
import Data.STRef
import Control.Monad.ST
import Data.Word (Word8, Word32)

import Game
import Draw

framesPerSecond :: Word32
framesPerSecond = 60

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

  putStrLn $ "Score: " ++ (show $ view gScore gs')
  
  hasQuit <- quitIfQPressed
  unless hasQuit (mainLoop gs' r)

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
  
