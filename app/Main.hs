{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (void)

import Brick.Main
import Brick.Widgets.List

import Checklist.Types
import Checklist.Attrs (attributesMap)
import Checklist.Events (eventHandler)
import Checklist.Draw (drawUI)

import qualified Data.Vector as Vec

initialState :: State
initialState = State { _checklist = list () (Vec.fromList ["something", "someting else", "another check"]) 1 }

app :: App State e ()
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = eventHandler
          , appStartEvent = return ()
          , appAttrMap = const attributesMap
          }

main :: IO ()
main = void $ defaultMain app initialState
