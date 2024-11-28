enum Dir { L, R, U, D }

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
  static func join() -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.JOIN
    return ret 
  
  
  # Constructor function for sum constructor LEAVE
  static func leave() -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.LEAVE
    return ret 
  
  
  # Constructor function for sum constructor MOVE
  static func move(fld_MOVE_0: Dir) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.MOVE
    ret.fld_MOVE_0 = fld_MOVE_0
    return ret 
  
  
  # Constructor function for sum constructor GET_STATE
  static func get_state() -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
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
  static func des(this: PackedByteArray) -> CliMsg:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = arr[0]
    match ret.con:
      Con.MOVE:   ret.fld_MOVE_0 = arr[1]
      
    
    return ret 
  
  
  # Serialize to binary
  static func ser(this: CliMsg) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
  
  
  # Serialize to array
  static func serToArr(this: CliMsg) -> Array[Variant]:
    match this.con:
      Con.JOIN:   return [ Con.JOIN ]  
      
      Con.LEAVE:   return [ Con.LEAVE ]  
      
      Con.MOVE:   return [ Con.MOVE, this.fld_MOVE_0 ]  
      
      Con.GET_STATE:   return [ Con.GET_STATE ]  
      
      _:  return [] 
      


class SrvMsg extends Object:

  class P_SockAddr_Pos_P extends Object:
  
    var snd: Pos
    var fst: SockAddr
  
    #  Equality check on type: P_SockAddr_Pos_P 
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

  #  Equality check on type: SrvMsg 
  static func eq(a: SrvMsg, b: SrvMsg) -> bool:
    return a.model.reduce(func(acc,x): return [ acc[0] && P_SockAddr_Pos_P.eq(x, b.model[acc[1]]), acc[1] + 1 ] , [true, 0])[0] 
  
  
  #  Non-static equality check of two SrvMsg 
  func eq1(b: SrvMsg) -> bool:
    return SrvMsg.eq(self, b) 
  
  
  # Constructor function for sum constructor PUT_STATE
  static func put_state(model: Array[P_SockAddr_Pos_P]) -> SrvMsg:
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
    ret.model.assign(arr.map(P_SockAddr_Pos_P.desFromArr))
    return ret 
  
  
  # Serialize to binary
  static func ser(this: SrvMsg) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
  
  
  # Serialize to array
  static func serToArr(this: SrvMsg) -> Array[Variant]:
    return this.model.map(func(x): return P_SockAddr_Pos_P.serToArr(x)) 


class Maybe extends Object:

  enum Con { Nothing, Just }

  var con: Con

  var fld_Just_0: Dir

  #  Equality check on type: Maybe 
  static func eq(a: Maybe, b: Maybe) -> bool:
    return a.con==b.con && (a.con==Con.Nothing || a.con==Con.Just && a.fld_Just_0==b.fld_Just_0) 
  
  
  #  Non-static equality check of two Maybe 
  func eq1(b: Maybe) -> bool:
    return Maybe.eq(self, b) 
  
  
  # Constructor function for sum constructor Nothing
  static func nothing() -> Maybe:
    var ret: Maybe = Maybe.new() 
    ret.con = Con.Nothing
    return ret 
  
  
  # Constructor function for sum constructor Just
  static func just(fld_Just_0: Dir) -> Maybe:
    var ret: Maybe = Maybe.new() 
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
  static func des(this: PackedByteArray) -> Maybe:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> Maybe:
    var ret: Maybe = Maybe.new() 
    ret.con = arr[0]
    match ret.con:
      Con.Just:   ret.fld_Just_0 = arr[1]
      
    
    return ret 
  
  
  # Serialize to binary
  static func ser(this: Maybe) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
  
  
  # Serialize to array
  static func serToArr(this: Maybe) -> Array[Variant]:
    match this.con:
      Con.Nothing:   return [ Con.Nothing ]  
      
      Con.Just:   return [ Con.Just, this.fld_Just_0 ]  
      
      _:  return [] 
      
