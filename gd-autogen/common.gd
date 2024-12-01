class Loc extends Object:

  static var m: int = 10
  static var n: int = 10

  var _mX: int
  var _mY: int

  #  Equality check on type: Loc 
  static func eq(a: Loc, b: Loc) -> bool:
    return a._mX==b._mX && a._mY==b._mY 
  
  
  #  Non-static equality check of two Loc 
  func eq1(b: Loc) -> bool:
    return Loc.eq(self, b) 
  
  
  # Constructor function for sum constructor Loc
  static func Loc(_mX: int, _mY: int) -> Loc:
    var ret: Loc = Loc.new() 
    ret._mX = _mX
    ret._mY = _mY
    return ret 
  
  
  # String representation of type
  func show() -> String:
    return "Loc" 
  
  
  # Deserialize from binary
  static func des(this: PackedByteArray) -> Loc:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> Loc:
    var ret: Loc = Loc.new() 
    ret._mX = arr[0]
    ret._mY = arr[1]
    return ret 
  
  
  # Serialize to binary
  static func ser(this: Loc) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
  
  
  # Serialize to array
  static func serToArr(this: Loc) -> Array[Variant]:
    return [ this._mX, this._mY ]  


class SockAddr extends Object:

  enum Con { SockAddrInet, SockAddrDummy }

  var con: Con


  var port: int
  var host: int

  #  Equality check on type: SockAddr 
  static func eq(a: SockAddr, b: SockAddr) -> bool:
    return a.con==b.con && (a.con==Con.SockAddrInet && a.port==b.port && a.host==b.host || a.con==Con.SockAddrDummy) 
  
  
  #  Non-static equality check of two SockAddr 
  func eq1(b: SockAddr) -> bool:
    return SockAddr.eq(self, b) 
  
  
  # Constructor function for sum constructor SockAddrInet
  static func SockAddrInet(port: int, host: int) -> SockAddr:
    var ret: SockAddr = SockAddr.new() 
    ret.con = Con.SockAddrInet
    ret.port = port
    ret.host = host
    return ret 
  
  
  # Constructor function for sum constructor SockAddrDummy
  static func SockAddrDummy() -> SockAddr:
    var ret: SockAddr = SockAddr.new() 
    ret.con = Con.SockAddrDummy
    return ret 
  
  
  # String representation of type
  func show() -> String:
    match self.con:
      Con.SockAddrInet:
        return "SockAddrInet" 
      
      Con.SockAddrDummy:
        return "SockAddrDummy" 
      
      _:  return "" 
      
  
  
  # Deserialize from binary
  static func des(this: PackedByteArray) -> SockAddr:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> SockAddr:
    var ret: SockAddr = SockAddr.new() 
    ret.con = arr[0]
    match ret.con:
      Con.SockAddrInet:
        ret.port = arr[1]
      
        ret.host = arr[2]
      
    
    return ret 
  
  
  # Serialize to binary
  static func ser(this: SockAddr) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
  
  
  # Serialize to array
  static func serToArr(this: SockAddr) -> Array[Variant]:
    match this.con:
      Con.SockAddrInet:
        return [ Con.SockAddrInet, this.port, this.host ]  
      
      Con.SockAddrDummy:
        return [ Con.SockAddrDummy ]  
      
      _:  return [] 
      


class SrvMsg extends Object:

  class P_SockAddr_Loc_P extends Object:
  
  
    var fst: SockAddr
    var snd: Loc
  
    #  Equality check on type: P_SockAddr_Loc_P 
    static func eq(a: P_SockAddr_Loc_P, b: P_SockAddr_Loc_P) -> bool:
      return SockAddr.eq(a.fst, b.fst) && Loc.eq(a.snd, b.snd) 
    
    
    #  Non-static equality check of two P_SockAddr_Loc_P 
    func eq1(b: P_SockAddr_Loc_P) -> bool:
      return P_SockAddr_Loc_P.eq(self, b) 
    
    
    # Constructor function for sum constructor P
    static func P(fst: SockAddr, snd: Loc) -> P_SockAddr_Loc_P:
      var ret: P_SockAddr_Loc_P = P_SockAddr_Loc_P.new() 
      ret.fst = fst
      ret.snd = snd
      return ret 
    
    
    # String representation of type
    func show() -> String:
      return "P" 
    
    
    # Deserialize from binary
    static func des(this: PackedByteArray) -> P_SockAddr_Loc_P:
      return desFromArr(bytes_to_var(this)) 
    
    
    # Deserialize from array
    static func desFromArr(arr: Array[Variant]) -> P_SockAddr_Loc_P:
      var ret: P_SockAddr_Loc_P = P_SockAddr_Loc_P.new() 
      ret.fst = SockAddr.desFromArr(arr[0])
      ret.snd = Loc.desFromArr(arr[1])
      return ret 
    
    
    # Serialize to binary
    static func ser(this: P_SockAddr_Loc_P) -> PackedByteArray:
      return var_to_bytes(serToArr(this)) 
    
    
    # Serialize to array
    static func serToArr(this: P_SockAddr_Loc_P) -> Array[Variant]:
      return [ SockAddr.serToArr(this.fst), Loc.serToArr(this.snd) ]  


  var model: Array[P_SockAddr_Loc_P]

  #  Equality check on type: SrvMsg 
  static func eq(a: SrvMsg, b: SrvMsg) -> bool:
    return a.model.reduce(func(acc,x): return [ acc[0] && P_SockAddr_Loc_P.eq(x, b.model[acc[1]]), acc[1] + 1 ] , [true, 0])[0] 
  
  
  #  Non-static equality check of two SrvMsg 
  func eq1(b: SrvMsg) -> bool:
    return SrvMsg.eq(self, b) 
  
  
  # Constructor function for sum constructor PUT_STATE
  static func PUT_STATE(model: Array[P_SockAddr_Loc_P]) -> SrvMsg:
    var ret: SrvMsg = SrvMsg.new() 
    ret.model = model
    return ret 
  
  
  # String representation of type
  func show() -> String:
    return "PUT_STATE" 
  
  
  # Deserialize from binary
  static func des(this: PackedByteArray) -> SrvMsg:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> SrvMsg:
    var ret: SrvMsg = SrvMsg.new() 
    ret.model.assign(arr.map(P_SockAddr_Loc_P.desFromArr))
    return ret 
  
  
  # Serialize to binary
  static func ser(this: SrvMsg) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
  
  
  # Serialize to array
  static func serToArr(this: SrvMsg) -> Array[Variant]:
    return this.model.map(func(x): return P_SockAddr_Loc_P.serToArr(x)) 


class CliMsg extends Object:

  enum Con { JOIN, LEAVE, MOVE, GET_STATE }

  var con: Con


  var fld_MOVE_0: Dir

  #  Equality check on type: CliMsg 
  static func eq(a: CliMsg, b: CliMsg) -> bool:
    return a.con==b.con && (a.con==Con.JOIN || a.con==Con.LEAVE || a.con==Con.MOVE && a.fld_MOVE_0==b.fld_MOVE_0 || a.con==Con.GET_STATE) 
  
  
  #  Non-static equality check of two CliMsg 
  func eq1(b: CliMsg) -> bool:
    return CliMsg.eq(self, b) 
  
  
  # Constructor function for sum constructor JOIN
  static func JOIN() -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.JOIN
    return ret 
  
  
  # Constructor function for sum constructor LEAVE
  static func LEAVE() -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.LEAVE
    return ret 
  
  
  # Constructor function for sum constructor MOVE
  static func MOVE(fld_MOVE_0: Dir) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.MOVE
    ret.fld_MOVE_0 = fld_MOVE_0
    return ret 
  
  
  # Constructor function for sum constructor GET_STATE
  static func GET_STATE() -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.GET_STATE
    return ret 
  
  
  # String representation of type
  func show() -> String:
    match self.con:
      Con.JOIN:
        return "JOIN" 
      
      Con.LEAVE:
        return "LEAVE" 
      
      Con.MOVE:
        return "MOVE" 
      
      Con.GET_STATE:
        return "GET_STATE" 
      
      _:  return "" 
      
  
  
  # Deserialize from binary
  static func des(this: PackedByteArray) -> CliMsg:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = arr[0]
    match ret.con:
      Con.MOVE:
        ret.fld_MOVE_0 = arr[1]
      
    
    return ret 
  
  
  # Serialize to binary
  static func ser(this: CliMsg) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
  
  
  # Serialize to array
  static func serToArr(this: CliMsg) -> Array[Variant]:
    match this.con:
      Con.JOIN:
        return [ Con.JOIN ]  
      
      Con.LEAVE:
        return [ Con.LEAVE ]  
      
      Con.MOVE:
        return [ Con.MOVE, this.fld_MOVE_0 ]  
      
      Con.GET_STATE:
        return [ Con.GET_STATE ]  
      
      _:  return [] 
      


enum Dir { L, R, U, D }

class MaybeDir extends Object:

  enum Con { Nothing, Just }

  var con: Con


  var fld_Just_0: Dir

  #  Equality check on type: MaybeDir 
  static func eq(a: MaybeDir, b: MaybeDir) -> bool:
    return a.con==b.con && (a.con==Con.Nothing || a.con==Con.Just && a.fld_Just_0==b.fld_Just_0) 
  
  
  #  Non-static equality check of two MaybeDir 
  func eq1(b: MaybeDir) -> bool:
    return MaybeDir.eq(self, b) 
  
  
  # Constructor function for sum constructor Nothing
  static func Nothing() -> MaybeDir:
    var ret: MaybeDir = MaybeDir.new() 
    ret.con = Con.Nothing
    return ret 
  
  
  # Constructor function for sum constructor Just
  static func Just(fld_Just_0: Dir) -> MaybeDir:
    var ret: MaybeDir = MaybeDir.new() 
    ret.con = Con.Just
    ret.fld_Just_0 = fld_Just_0
    return ret 
  
  
  # String representation of type
  func show() -> String:
    match self.con:
      Con.Nothing:
        return "Nothing" 
      
      Con.Just:
        return "Just" 
      
      _:  return "" 
      
  
  
  # Deserialize from binary
  static func des(this: PackedByteArray) -> MaybeDir:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> MaybeDir:
    var ret: MaybeDir = MaybeDir.new() 
    ret.con = arr[0]
    match ret.con:
      Con.Just:
        ret.fld_Just_0 = arr[1]
      
    
    return ret 
  
  
  # Serialize to binary
  static func ser(this: MaybeDir) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
  
  
  # Serialize to array
  static func serToArr(this: MaybeDir) -> Array[Variant]:
    match this.con:
      Con.Nothing:
        return [ Con.Nothing ]  
      
      Con.Just:
        return [ Con.Just, this.fld_Just_0 ]  
      
      _:  return [] 
      
