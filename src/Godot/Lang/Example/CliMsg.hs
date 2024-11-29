
module Godot.Lang.Example.CliMsg where

import GHC.Generics (Generic)
import Godot.Lang.Class (ToDefCls)

-- data CliMsg
--   = JOIN
--   | LEAVE String
--   | ACTION { act :: Action , time :: Int, vec :: V2 Double}
--   | GET_LISTS { glList :: [[Float]], glList2 :: [Action], glList3 :: [[[Action]]] }
--   | GET_MAP { gmMap :: Map Float String}
--   deriving (Show, Generic)
--
-- data Action = MOVE Int
--             | FIRE Int
--   deriving (Show, Eq, Read, Generic)

data Dir = L | R | U | D deriving (Show, Eq, Generic, Enum, Bounded, Read)

data CliMsg
  = JOIN
  | LEAVE
  | MOVE Dir
  | GET_STATE
  deriving (Show, Eq, Read, Generic)

instance ToDefCls CliMsg
