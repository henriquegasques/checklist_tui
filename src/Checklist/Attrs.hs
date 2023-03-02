module Checklist.Attrs (customAttr, attributesMap) where

import qualified Graphics.Vty as V
import Brick.Widgets.List
import Brick.AttrMap (attrMap, AttrMap, attrName, AttrName)
import Brick.Util (fg, on)

customAttr :: AttrName
customAttr = listSelectedAttr <> attrName "custom"

attributesMap :: AttrMap
attributesMap = attrMap V.defAttr [ (listAttr,            V.white `on` V.blue)
                                  , (listSelectedAttr,    V.blue `on` V.white)
                                  , (customAttr,          fg V.cyan)
                                  ]

