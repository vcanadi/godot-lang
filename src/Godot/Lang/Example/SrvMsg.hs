{-# LANGUAGE TemplateHaskell #-}

module Godot.Lang.Example.SrvMsg where

import Godot.Lang.Trans
import Linear.V2 (V2)
import Data.Map (Map)
import GHC.Generics (Generic)
import Language.Haskell.TH (Q, Exp, runIO)
import Godot.Lang.Core (ToTyp (toTyp), Typ (TypPrim), PrimTyp (PTString))
import Data.Word (Word16, Word32)

newtype PortNumber = PortNum Word16 deriving (Eq, Ord, Show, Generic)
type HostAddress = Word32
type FlowInfo = Word32
type ScopeID = Word32
type HostAddress6 = (Word32, Word32, Word32, Word32)

data SockAddr
  = SockAddrInet
        PortNumber      -- sin_port
        HostAddress     -- sin_addr  (ditto)
  | SockAddrInet6
        PortNumber      -- sin6_port
        FlowInfo        -- sin6_flowinfo (ditto)
        HostAddress6    -- sin6_addr (ditto)
        ScopeID         -- sin6_scope_id (ditto)
  -- | The path must have fewer than 104 characters. All of these characters must have code points less than 256.
  | SockAddrUnix
        String           -- sun_path
  deriving (Eq, Ord, Show, Generic)

data SrvMsg = PUT_STATE { model :: Model }
  deriving (Show, Eq, Generic)


-- | State of the game (client info and board coordinates)
type Model = Map SockAddr Pos

data Pos = Pos
  { _mX :: Int
  , _mY :: Int
  } deriving (Show, Eq, Generic)

generateGDScript :: Q Exp
generateGDScript = do
    runIO $ genGDScript @SrvMsg "./gd-autogen"
    [| "This string is generated at compile-time." |]

