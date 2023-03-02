module Checklist.Draw (drawUI) where

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Center

import Checklist.Types
import Checklist.Attrs (customAttr)

import Lens.Micro ((^.))
import qualified Data.Vector as Vec

drawUI :: State -> [Widget ()]
drawUI s = [ui]
    where
        l = s^.checklist
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.listSelectedL of
                Nothing -> str "-"
                Just i  -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.listElementsL
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
    in hCenter $ str "Item " <+> selStr (show a)
