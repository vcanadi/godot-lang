class_name SrvMsg extends object

  enum Con { PUT_STATE }

  var fld_PUT_STATE_0: Dictionary[SockAddr, Dictionary[String, Loc]]

  # Constructor function for sum constructor PUT_STATE
  static func put_state(var fld_PUT_STATE_0: Dictionary[SockAddr, Dictionary[String, Loc]]) -> SrvMsg:
    var ret: SrvMsg = SrvMsg.new() 
    ret.con = Con.PUT_STATE
    ret.fld_PUT_STATE_0 = fld_PUT_STATE_0
    return ret 
  
  # String representation of type
  func show() -> String:
    match self.con:
      Con.PUT_STATE:  return "PUT_STATE"  
  
  # Deserialize from array
  static func desFromArr(var arr: Array[Variant]) -> SrvMsg:
    var ret: SrvMsg = SrvMsg.new() 
    ret.con = arr[0]
    match ret.con:
      Con.PUT_STATE:
        var dict_A: Dictionary[SockAddr, Dictionary[String, Loc]] = {} 
        var k_A: SockAddr
        var v_A: Dictionary[String, Loc]
        
        for pair_A in arr[1]:
          k_A = pair_A[0].desFromArr()
          var dict_B: Dictionary[String, Loc] = {} 
          var k_B: String
          var v_B: Loc
          
          for pair_B in pair_A[1]:
            k_B = pair_B[0]
            v_B = pair_B[1].desFromArr()
            dict_B[k_B] = v_B 
          v_A = dict_B
          dict_A[k_A] = v_A 
        ret.fld_PUT_STATE_0 = dict_A 
    return ret 
  
  # Deserialize from binary
  static func des(var this: PackedByteArray) -> SrvMsg:
    return desFromArr(bytes_to_var(bs)) 
  
  # Serialize to array
  static func serToArr(var this: SrvMsg) -> Array[Variant]:
    match this.con:
      Con.PUT_STATE:  return  [ Con.PUT_STATE, fld_PUT_STATE_0.keys().map(func(k):  [ k.serToArr(), fld_PUT_STATE_0[k].keys().map(func(k):  [ k, fld_PUT_STATE_0[k][k].serToArr() ] ) ] ) ]   
  
  # Serialize to binary
  static func ser(var this: SrvMsg) -> PackedByteArray:
    return var_to_bytes(serToArr(this))  