class SrvMsg extends Object:

  class P_SockAddr_Pos_P extends Object:
  
    var fst: SockAddr
    var snd: Pos
  
    # Constructor function for sum constructor P
    static func p(fst: SockAddr, snd: Pos) -> P_SockAddr_Pos_P:
      var ret: P_SockAddr_Pos_P = P_SockAddr_Pos_P.new() 
      ret.fst = fst
      ret.snd = snd
      return ret 
    
    
    # String representation of type
    func show() -> String:
      return "P" 
    
    
    # Deserialize from array
    static func desFromArr(arr: Array[Variant]) -> P_SockAddr_Pos_P:
      var ret: P_SockAddr_Pos_P = P_SockAddr_Pos_P.new() 
      ret.fst = SockAddr.desFromArr(arr[0])
      ret.snd = Pos.desFromArr(arr[1])
      return ret 
    
    
    # Deserialize from binary
    static func des(this: PackedByteArray) -> P_SockAddr_Pos_P:
      return desFromArr(bytes_to_var(this)) 
    
    
    # Serialize to array
    static func serToArr(this: P_SockAddr_Pos_P) -> Array[Variant]:
      return  [ SockAddr.serToArr(this.this.fst), Pos.serToArr(this.this.snd) ]  
    
    
    # Serialize to binary
    static func ser(this: P_SockAddr_Pos_P) -> PackedByteArray:
      return var_to_bytes(serToArr(this)) 

  var model: Array[P_SockAddr_Pos_P]

  # Constructor function for sum constructor PUT_STATE
  static func put_state(model: Array[P_SockAddr_Pos_P]) -> SrvMsg:
    var ret: SrvMsg = SrvMsg.new() 
    ret.model = model
    return ret 
  
  
  # String representation of type
  func show() -> String:
    return "PUT_STATE" 
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> SrvMsg:
    var ret: SrvMsg = SrvMsg.new() 
    ret.model.assign(arr[0].map(func(x): [arr[0][0], arr[0][1]]))
    return ret 
  
  
  # Deserialize from binary
  static func des(this: PackedByteArray) -> SrvMsg:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Serialize to array
  static func serToArr(this: SrvMsg) -> Array[Variant]:
    return  [ this.model.map(func(x): P_SockAddr_Pos_P.serToArr(x)) ]  
  
  
  # Serialize to binary
  static func ser(this: SrvMsg) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
