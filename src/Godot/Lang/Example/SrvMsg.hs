{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Godot.Lang.Example.SrvMsg where

import Data.Map (Map)
import GHC.Generics (Generic)
import Godot.Lang.Class (ToDC(extraStatVars, extraFuncs))
import Godot.Lang.Core
import Data.String.Interpolate (i)

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

instance ToDC SockAddr

data SrvMsg = PUT_STATE { model :: Model }
  deriving (Show, Eq, Generic)

instance ToDC SrvMsg where
  extraFuncs _ =
    [ func "display" [] (TypPrim PTString)
        [ StmtRaw srvMsgDisplayFuncBody ]
    ]

srvMsgDisplayFuncBody =
  [i|
  var s: String = ""
  for _j in range(Loc.m-1,-1,-1):
    for _i in range(Loc.n):
      s += ("X" if model.any(func(ci): return ci.snd._mX == _i and ci.snd._mY == _j) else " ") + "|"
    s+="\\n"
  return s
  |]

-- | State of the game (client info and board coordinates)
type Model = Map SockAddr Loc

data Loc = Loc
  { _mX :: Int
  , _mY :: Int
  } deriving (Show, Eq, Generic)

instance ToDC Loc where
  extraStatVars _ = [ DefVar (VarName "m") (TypPrim PTInt) (Just "10")
                    , DefVar (VarName "n") (TypPrim PTInt) (Just "10")
                    ]
  extraFuncs _ =
    [ func "display" [] (TypPrim PTString)
        [ StmtRaw locDisplayFuncBody ]
    ]

locDisplayFuncBody =
  [i|
  var s: String = ""
  for _j in range(m-1,-1,-1):
    for _i in range(n):
      s += ("X" if _mX == _i and _mY == _j else " ") + "|"
    s+="\\n"
  return s
  |]
