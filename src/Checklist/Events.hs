{-# LANGUAGE TemplateHaskell #-}

module Checklist.Events (eventHandler) where

import Brick.Main (halt)
import Brick.Types (BrickEvent(..), EventM, zoom)
import Brick.Widgets.List

import qualified Graphics.Vty as V

import Checklist.Types (State(..), checklist)

eventHandler :: BrickEvent () e -> EventM () State ()
eventHandler (VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> halt
    ev -> zoom checklist $ handleListEventVi handleListEvent ev
eventHandler _ = return ()
