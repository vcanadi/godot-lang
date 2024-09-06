class_name SrvMsg extends object

  enum Con { PUT_STATE }

  var fld_PUT_STATE_0: Dictionary[String, Loc]
  var fld_PUT_STATE_1: Array[Action]

  # Constructor function for sum constructor PUT_STATE
  static func put_state(var fld_PUT_STATE_0: Dictionary[String, Loc], var fld_PUT_STATE_1: Array[Action]) -> SrvMsg:
    var ret: SrvMsg = SrvMsg.new() 
    ret.con = Con.PUT_STATE
    ret.fld_PUT_STATE_0 = fld_PUT_STATE_0
    ret.fld_PUT_STATE_1 = fld_PUT_STATE_1
    return ret 
  
  # String representation of type
  func show() -> String:
    match self.con:
      Con.PUT_STATE:  return "PUT_STATE"  
  
  # Deserialize from array
  static func desFromArr(var arr: Array[Variant]) -> SrvMsg:
    var ret: SrvMsg = SrvMsg.new() 
    ret.con = arr[0]
    match arr[0]:
      Con.PUT_STATE:
        ret.fld_PUT_STATE_0 = arr[1]
        ret.fld_PUT_STATE_1.assign(arr[2].map(desFromArr)) 
    return ret 
  
  # Deserialize from binary
  static func des(var this: PackedByteArray) -> SrvMsg:
    return desFromArr(bytes_to_var(bs)) 
  
  # Serialize to array
  static func serToArr(var this: SrvMsg) -> Array[Variant]:
    match this.con:
      Con.PUT_STATE:  return  [ Con.PUT_STATE, fld_PUT_STATE_0, fld_PUT_STATE_1.map(serToArr) ]   
  
  # Serialize to binary
  static func ser(var this: SrvMsg) -> PackedByteArray:
    return var_to_bytes(serToArr(this))  