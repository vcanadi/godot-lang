{-# LANGUAGE TemplateHaskell #-}

module Godot.Lang.Example.SrvMsg where

import Godot.Lang.Trans
import Data.Map (Map)
import GHC.Generics (Generic)
import Language.Haskell.TH (Q, Exp, runIO)

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

data SrvMsg = PUT_STATE { model :: Model }
  deriving (Show, Eq, Generic)


-- | State of the game (client info and board coordinates)
type Model = Map SockAddr Loc

data Loc = Loc
  { _mX :: Int
  , _mY :: Int
  } deriving (Show, Eq, Generic)

generateGDScript :: Q Exp
generateGDScript = do
    runIO $ genGDScript @SrvMsg "./gd-autogen"
    [| "This string is generated at compile-time." |]

