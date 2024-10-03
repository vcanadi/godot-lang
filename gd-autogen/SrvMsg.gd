class SrvMsg extends Object:

  class P_SockAddr_Pos_P extends Object:
  
    var fst: SockAddr
    var snd: Pos
  
    #  Equality check of two P_SockAddr_Pos_P 
    static func eq(a: P_SockAddr_Pos_P, b: P_SockAddr_Pos_P) -> bool:
      return SockAddr.eq(a.fst, b.fst) && Pos.eq(a.snd, b.snd) 
    
    
    #  Non-static equality check of two P_SockAddr_Pos_P 
    func eq1(b: P_SockAddr_Pos_P) -> bool:
      return P_SockAddr_Pos_P.eq(self, b) 
    
    
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
    
    
    # Serialize to array
    static func serToArr(this: P_SockAddr_Pos_P) -> Array[Variant]:
      return [ SockAddr.serToArr(this.fst), Pos.serToArr(this.snd) ]  

  var model: Array[P_SockAddr_Pos_P]

  #  Equality check of two SrvMsg 
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
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> SrvMsg:
    var ret: SrvMsg = SrvMsg.new() 
    ret.model.assign(arr.map(P_SockAddr_Pos_P.desFromArr))
    return ret 
  
  
  # Serialize to array
  static func serToArr(this: SrvMsg) -> Array[Variant]:
    return this.model.map(func(x): return P_SockAddr_Pos_P.serToArr(x)) 
