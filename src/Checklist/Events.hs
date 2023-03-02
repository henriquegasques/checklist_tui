module Checklist.Events (eventHandler) where

import Brick.Main (halt)
import Brick.Types (BrickEvent(..), EventM, zoom, modify)
import Brick.Widgets.List

import qualified Graphics.Vty as V
import Data.Text (Text, replace, pack, isPrefixOf)

import Checklist.Types (State(..), checklist)

eventHandler :: BrickEvent () e -> EventM () State ()
eventHandler (VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> halt
    V.EvKey (V.KChar 'x') [] -> zoom checklist $ modify $ listModify toggleChecked
    ev -> zoom checklist $ handleListEventVi handleListEvent ev
eventHandler _ = return ()

toggleChecked :: Text -> Text
toggleChecked item
  | pack "-[x]" `isPrefixOf` item = replace (pack "[x]") (pack "[ ]") item
  | otherwise                     = replace (pack "[ ]") (pack "[x]") item
