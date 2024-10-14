enum Dir { L, R, U, D }

class CliMsgDir extends Object:

  enum Con { JOIN, LEAVE, MOVE, GET_STATE }

  var con: Con

  var fld_MOVE_0: Dir

  #  Equality check of two CliMsgDir 
  static func eq(a: CliMsgDir, b: CliMsgDir) -> bool:
    return a.con==b.con && (a.con==Con.JOIN || a.con==Con.LEAVE || a.con==Con.MOVE && a.fld_MOVE_0==b.fld_MOVE_0 || a.con==Con.GET_STATE) 
  
  
  #  Non-static equality check of two CliMsgDir 
  func eq1(b: CliMsgDir) -> bool:
    return CliMsgDir.eq(self, b) 
  
  
  # Constructor function for sum constructor JOIN
  static func join() -> CliMsgDir:
    var ret: CliMsgDir = CliMsgDir.new() 
    ret.con = Con.JOIN
    return ret 
  
  
  # Constructor function for sum constructor LEAVE
  static func leave() -> CliMsgDir:
    var ret: CliMsgDir = CliMsgDir.new() 
    ret.con = Con.LEAVE
    return ret 
  
  
  # Constructor function for sum constructor MOVE
  static func move(fld_MOVE_0: Dir) -> CliMsgDir:
    var ret: CliMsgDir = CliMsgDir.new() 
    ret.con = Con.MOVE
    ret.fld_MOVE_0 = fld_MOVE_0
    return ret 
  
  
  # Constructor function for sum constructor GET_STATE
  static func get_state() -> CliMsgDir:
    var ret: CliMsgDir = CliMsgDir.new() 
    ret.con = Con.GET_STATE
    return ret 
  
  
  # String representation of type
  func show() -> String:
    match self.con:
      Con.JOIN:   return "JOIN" 
      
      Con.LEAVE:   return "LEAVE" 
      
      Con.MOVE:   return "MOVE" 
      
      Con.GET_STATE:   return "GET_STATE" 
      
      _:  return "" 
      
  
  
  # Deserialize from binary
  static func des(this: PackedByteArray) -> CliMsgDir:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> CliMsgDir:
    var ret: CliMsgDir = CliMsgDir.new() 
    ret.con = arr[0]
    match ret.con:
      Con.MOVE:   ret.fld_MOVE_0 = arr[1]
      
    
    return ret 
  
  
  # Serialize to binary
  static func ser(this: CliMsgDir) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
  
  
  # Serialize to array
  static func serToArr(this: CliMsgDir) -> Array[Variant]:
    match this.con:
      Con.JOIN:   return [ Con.JOIN ]  
      
      Con.LEAVE:   return [ Con.LEAVE ]  
      
      Con.MOVE:   return [ Con.MOVE, this.fld_MOVE_0 ]  
      
      Con.GET_STATE:   return [ Con.GET_STATE ]  
      
      _:  return [] 
      


class SrvMsgMap SockAddr Pos extends Object:

  class P_SockAddr_Pos_P extends Object:
  
    var snd: Pos
    var fst: SockAddr
  
    #  Equality check of two P_SockAddr_Pos_P 
    static func eq(a: P_SockAddr_Pos_P, b: P_SockAddr_Pos_P) -> bool:
      return Pos.eq(a.snd, b.snd) && SockAddr.eq(a.fst, b.fst) 
    
    
    #  Non-static equality check of two P_SockAddr_Pos_P 
    func eq1(b: P_SockAddr_Pos_P) -> bool:
      return P_SockAddr_Pos_P.eq(self, b) 
    
    
    # Constructor function for sum constructor P
    static func p(snd: Pos, fst: SockAddr) -> P_SockAddr_Pos_P:
      var ret: P_SockAddr_Pos_P = P_SockAddr_Pos_P.new() 
      ret.snd = snd
      ret.fst = fst
      return ret 
    
    
    # String representation of type
    func show() -> String:
      return "P" 
    
    
    # Deserialize from binary
    static func des(this: PackedByteArray) -> P_SockAddr_Pos_P:
      return desFromArr(bytes_to_var(this)) 
    
    
    # Deserialize from array
    static func desFromArr(arr: Array[Variant]) -> P_SockAddr_Pos_P:
      var ret: P_SockAddr_Pos_P = P_SockAddr_Pos_P.new() 
      ret.snd = Pos.desFromArr(arr[0])
      ret.fst = SockAddr.desFromArr(arr[1])
      return ret 
    
    
    # Serialize to binary
    static func ser(this: P_SockAddr_Pos_P) -> PackedByteArray:
      return var_to_bytes(serToArr(this)) 
    
    
    # Serialize to array
    static func serToArr(this: P_SockAddr_Pos_P) -> Array[Variant]:
      return [ Pos.serToArr(this.snd), SockAddr.serToArr(this.fst) ]  

  var model: Array[P_SockAddr_Pos_P]

  #  Equality check of two SrvMsgMap SockAddr Pos 
  static func eq(a: SrvMsgMap SockAddr Pos, b: SrvMsgMap SockAddr Pos) -> bool:
    return a.model.reduce(func(acc,x): return [ acc[0] && P_SockAddr_Pos_P.eq(x, b.model[acc[1]]), acc[1] + 1 ] , [true, 0])[0] 
  
  
  #  Non-static equality check of two SrvMsgMap SockAddr Pos 
  func eq1(b: SrvMsgMap SockAddr Pos) -> bool:
    return SrvMsgMap SockAddr Pos.eq(self, b) 
  
  
  # Constructor function for sum constructor PUT_STATE
  static func put_state(model: Array[P_SockAddr_Pos_P]) -> SrvMsgMap SockAddr Pos:
    var ret: SrvMsgMap SockAddr Pos = SrvMsgMap SockAddr Pos.new() 
    ret.model = model
    return ret 
  
  
  # String representation of type
  func show() -> String:
    return "PUT_STATE" 
  
  
  # Deserialize from binary
  static func des(this: PackedByteArray) -> SrvMsgMap SockAddr Pos:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> SrvMsgMap SockAddr Pos:
    var ret: SrvMsgMap SockAddr Pos = SrvMsgMap SockAddr Pos.new() 
    ret.model.assign(arr.map(P_SockAddr_Pos_P.desFromArr))
    return ret 
  
  
  # Serialize to binary
  static func ser(this: SrvMsgMap SockAddr Pos) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
  
  
  # Serialize to array
  static func serToArr(this: SrvMsgMap SockAddr Pos) -> Array[Variant]:
    return this.model.map(func(x): return P_SockAddr_Pos_P.serToArr(x)) 


class MaybeDir extends Object:

  enum Con { Nothing, Just }

  var con: Con

  var fld_Just_0: Dir

  #  Equality check of two MaybeDir 
  static func eq(a: MaybeDir, b: MaybeDir) -> bool:
    return a.con==b.con && (a.con==Con.Nothing || a.con==Con.Just && a.fld_Just_0==b.fld_Just_0) 
  
  
  #  Non-static equality check of two MaybeDir 
  func eq1(b: MaybeDir) -> bool:
    return MaybeDir.eq(self, b) 
  
  
  # Constructor function for sum constructor Nothing
  static func nothing() -> MaybeDir:
    var ret: MaybeDir = MaybeDir.new() 
    ret.con = Con.Nothing
    return ret 
  
  
  # Constructor function for sum constructor Just
  static func just(fld_Just_0: Dir) -> MaybeDir:
    var ret: MaybeDir = MaybeDir.new() 
    ret.con = Con.Just
    ret.fld_Just_0 = fld_Just_0
    return ret 
  
  
  # String representation of type
  func show() -> String:
    match self.con:
      Con.Nothing:   return "Nothing" 
      
      Con.Just:   return "Just" 
      
      _:  return "" 
      
  
  
  # Deserialize from binary
  static func des(this: PackedByteArray) -> MaybeDir:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> MaybeDir:
    var ret: MaybeDir = MaybeDir.new() 
    ret.con = arr[0]
    match ret.con:
      Con.Just:   ret.fld_Just_0 = arr[1]
      
    
    return ret 
  
  
  # Serialize to binary
  static func ser(this: MaybeDir) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
  
  
  # Serialize to array
  static func serToArr(this: MaybeDir) -> Array[Variant]:
    match this.con:
      Con.Nothing:   return [ Con.Nothing ]  
      
      Con.Just:   return [ Con.Just, this.fld_Just_0 ]  
      
      _:  return [] 
      
