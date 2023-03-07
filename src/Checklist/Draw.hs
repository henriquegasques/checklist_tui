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
        label = str "Checklist TUI - Item " <+> cur <+> str " of " <+> total
        cur = case l^.listSelectedL of
                Nothing -> str "-"
                Just i  -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.listElementsL
        isListFocused = True
        box = borderWithLabel label $
              hLimit 70 $
              vLimit 35 $
              renderList drawListElement isListFocused l
        ui = vCenter $ vBox [ hCenter box
                            , str " "
                            , hCenter $ str "Press x to check/uncheck element."
                            , hCenter $ str "Press Esc to exit."
                            ]

drawListElement :: (Show a) => Bool -> a -> Widget ()
drawListElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str s)
                   else str s
    in padLeftRight 5 $ selStr (show a)
