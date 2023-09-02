{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (hFlush, stdout)
import System.Console.Haskeline
import Brick
import Cob
import Cob.UserM as UM
import Data.Void

import qualified Data.Vector as V
import Examinator.View
import Examinator

import Brick.BChan
import Brick.Widgets.List as L
import Control.Concurrent
import Control.Monad

import System.Console.ANSI

import qualified Graphics.Vty as V
import GHC.IO.Handle

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
-- data Tick = Tick

main :: IO ()
main = do
  -- Old brick stuff
  -- vty1 <- V.mkVty V.defaultConfig
  -- chan <- newBChan 10
  -- forkIO $ forever $ do
  --   writeBChan chan Tick
  --   threadDelay 100000 -- decides how fast your game moves
  -- void $ customMain vty1 (V.mkVty V.defaultConfig) (Just chan) examinatorApp (AppState (L.list () (V.fromList tests) (length tests)))

  putStr "Insert your username: " *> hFlush stdout
  user <- getLine
  Just pass <- runInputT defaultSettings $ getPassword Nothing "And your password: "

  sess <- UM.umSession "learning.cultofbits.com" user pass

  -- Start testing procedure:
  clearScreen
  setCursorPosition 0 0


  let 
    validate :: Examinator Bool -> IO ()
    validate act = do
      threadDelay 1000000
      x <- runCob sess act
      if x then return ()
           else validate act

  -- Worry about colors later...
  forM_ tests $ \(Test title prompts) -> do
    titleT title
    forM_ prompts $ \(Prompt txt test,_) -> do
      promptT txt
      validate test
      successT

titleT :: String -> IO ()
titleT = putStrLn

promptT :: String -> IO ()
promptT str = do
  putStrLn (replicate 4 ' ' <> "* " <> str)
  putStr (replicate 4 ' ')
  hFlush stdout

successT :: IO ()
successT = do
  putStrLn $ replicate 4 ' ' <> "Great job!"

