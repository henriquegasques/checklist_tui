{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Checklist.Types (
                         State(..)
                       , checklist
                       ) where

import Lens.Micro.TH (makeLenses)
import Brick.Widgets.List

data State = State { _checklist :: List () String }

makeLenses ''State

