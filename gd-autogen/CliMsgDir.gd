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
      
