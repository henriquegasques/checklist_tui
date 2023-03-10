module Main (main) where

import Control.Monad (void)

import Brick.Main
import Brick.Widgets.List

import Checklist.Types
import Checklist.Attrs (attributesMap)
import Checklist.Events (eventHandler)
import Checklist.Draw (drawUI)

import Data.Text (pack)
import qualified Data.Vector as Vec

initialState :: State
initialState =
  let checks = Vec.fromList [ pack "-[ ] Reviewed changes locally before commiting"
                            , pack "-[ ] All changes are covered by tests"
                            , pack "-[ ] Ran CI checks locally and they are green"
                            ]
  in State { _checklist = list () checks 1 }

app :: App State e ()
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = eventHandler
          , appStartEvent = return ()
          , appAttrMap = const attributesMap
          }

main :: IO ()
main = void $ defaultMain app initialState
