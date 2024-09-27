class SrvMsg extends Object:

  class P_SockAddr_Pos_P extends Object:
  
    enum Con { P }
  
    var con: Con
  
    var fst: SockAddr
    var snd: Pos
  
    # Constructor function for sum constructor P
    static func p(fst: SockAddr, snd: Pos) -> P_SockAddr_Pos_P:
      var ret: P_SockAddr_Pos_P = P_SockAddr_Pos_P.new() 
      ret.con = Con.P
      ret.fst = fst
      ret.snd = snd
      return ret 
    
    
    # String representation of type
    func show() -> String:
      match self.con:  
        Con.P:
          return "P" 
        
        _:
          return "" 
        
    
    
    # Deserialize from array
    static func desFromArr(arr: Array[Variant]) -> P_SockAddr_Pos_P:
      var ret: P_SockAddr_Pos_P = P_SockAddr_Pos_P.new() 
      ret.con = arr[0]
      match ret.con:  
        Con.P:
          ret.fst = SockAddr.desFromArr(arr[1])
        
          ret.snd = Pos.desFromArr(arr[2])
        
      
      return ret 
    
    
    # Deserialize from binary
    static func des(this: PackedByteArray) -> P_SockAddr_Pos_P:
      return desFromArr(bytes_to_var(this)) 
    
    
    # Serialize to array
    static func serToArr(this: P_SockAddr_Pos_P) -> Array[Variant]:
      match this.con:  
        Con.P:
          return  [ Con.P, SockAddr.serToArr(this.fst), Pos.serToArr(this.snd) ]  
        
        _:
          return [] 
        
    
    
    # Serialize to binary
    static func ser(this: P_SockAddr_Pos_P) -> PackedByteArray:
      return var_to_bytes(serToArr(this)) 

  enum Con { PUT_STATE }

  var con: Con

  var model: Array[P_SockAddr_Pos_P]

  # Constructor function for sum constructor PUT_STATE
  static func put_state(model: Array[P_SockAddr_Pos_P]) -> SrvMsg:
    var ret: SrvMsg = SrvMsg.new() 
    ret.con = Con.PUT_STATE
    ret.model = model
    return ret 
  
  
  # String representation of type
  func show() -> String:
    match self.con:  
      Con.PUT_STATE:
        return "PUT_STATE" 
      
      _:
        return "" 
      
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> SrvMsg:
    var ret: SrvMsg = SrvMsg.new() 
    ret.con = arr[0]
    match ret.con:  
      Con.PUT_STATE:
        ret.model.assign(arr[1].map(func(x): [arr[1][0], arr[1][1]]))
      
    
    return ret 
  
  
  # Deserialize from binary
  static func des(this: PackedByteArray) -> SrvMsg:
    return desFromArr(bytes_to_var(this)) 
  
  
  # Serialize to array
  static func serToArr(this: SrvMsg) -> Array[Variant]:
    match this.con:  
      Con.PUT_STATE:
        return  [ Con.PUT_STATE, model.map(func(x):  [ SockAddr.serToArr(this.x.fst), Pos.serToArr(this.x.snd) ] ) ]  
      
      _:
        return [] 
      
  
  
  # Serialize to binary
  static func ser(this: SrvMsg) -> PackedByteArray:
    return var_to_bytes(serToArr(this)) 
