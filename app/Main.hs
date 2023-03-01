{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where

import Control.Monad (void)

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

import qualified Graphics.Vty as V

import Brick
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Center

import Ui

import qualified Data.Vector as Vec

data State = State { _message :: String
                   , _checklist :: List () String
                   }

makeLenses ''State

customAttr :: AttrName
customAttr = listSelectedAttr <> attrName "custom"

attributesMap :: AttrMap
attributesMap = attrMap V.defAttr [ (listAttr,            V.white `on` V.blue)
                                  , (listSelectedAttr,    V.blue `on` V.white)
                                  , (customAttr,            fg V.cyan)
                                  ]

drawUI :: State -> [Widget ()]
drawUI s = [ui]
    where
        l = s^.checklist
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.(listSelectedL) of
                Nothing -> str "-"
                Just i  -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.(listElementsL)
        box = borderWithLabel label $
              hLimit 25 $
              vLimit 15 $
              renderList listDrawElement True l
        ui = vCenter $ vBox [ hCenter box
                              , str " "
                              , hCenter $ str "Press +/- to add/remove list elements."
                              , hCenter $ str "Press Esc to exit."
                              ]

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in hCenter $ str "Item " <+> (selStr $ show a)

eventHandler :: BrickEvent () e -> EventM () State ()
eventHandler (VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> halt
    ev -> zoom checklist $ handleListEventVi handleListEvent ev
eventHandler _ = return ()

initialState :: State
initialState = State { _message = Ui.title
                     , _checklist = list () (Vec.fromList ["something", "someting else", "another check"]) 1
                     }

app :: App State e ()
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = eventHandler
          , appStartEvent = return ()
          , appAttrMap = const attributesMap
          }

main :: IO ()
main = void $ defaultMain app initialState
