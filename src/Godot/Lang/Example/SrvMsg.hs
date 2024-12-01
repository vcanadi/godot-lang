{-# LANGUAGE TemplateHaskell #-}

module Godot.Lang.Example.SrvMsg where

import Data.Map (Map)
import GHC.Generics (Generic)
import Godot.Lang.Class (ToDefCls(extraStatVars))
import Godot.Lang.Core

type PortNumber = Int
type HostAddress = Int
type FlowInfo = Int
type ScopeID = Int
type HostAddress6 = (Int, Int)

data SockAddr
  = SockAddrInet
    { port :: PortNumber      -- sin_port
    , host :: HostAddress     -- sin_addr  (ditto)
    }
  | SockAddrDummy
  deriving (Eq, Ord, Show, Generic)

instance ToDefCls SockAddr

data SrvMsg = PUT_STATE { model :: Model }
  deriving (Show, Eq, Generic)

instance ToDefCls SrvMsg

-- | State of the game (client info and board coordinates)
type Model = Map SockAddr Loc

data Loc = Loc
  { _mX :: Int
  , _mY :: Int
  } deriving (Show, Eq, Generic)

instance ToDefCls Loc where
  extraStatVars _ = [ "m" -:: TypPrim PTInt
                    , "n" -:: TypPrim PTInt
                    ]
