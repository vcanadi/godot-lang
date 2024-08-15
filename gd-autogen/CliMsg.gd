class_name CliMsg extends object

  enum Con { ACTION, BLA, GET_STATE, GET_STATE2, JOIN, LEAVE, NEWCON }

  var field_ACTION_act: Action
  var field_ACTION_integ: int
  var field_BLA_xxx: String
  var field_GET_STATE_st: Array
  var field_GET_STATE2_st2: Dictionary
  var field_LEAVE: float
  var field_NEWCON_vec: Vector2

  static func action(var field_ACTION_act: Action, var field_ACTION_integ: int) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.ACTION
    ret.field_ACTION_act = field_ACTION_act
    ret.field_ACTION_integ = field_ACTION_integ
    return ret 
  
  static func bla(var field_BLA_xxx: String) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.BLA
    ret.field_BLA_xxx = field_BLA_xxx
    return ret 
  
  static func get_state(var field_GET_STATE_st: Array) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.GET_STATE
    ret.field_GET_STATE_st = field_GET_STATE_st
    return ret 
  
  static func get_state2(var field_GET_STATE2_st2: Dictionary) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.GET_STATE2
    ret.field_GET_STATE2_st2 = field_GET_STATE2_st2
    return ret 
  
  static func join() -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.JOIN
    return ret 
  
  static func leave(var field_LEAVE: float) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.LEAVE
    ret.field_LEAVE = field_LEAVE
    return ret 
  
  static func newcon(var field_NEWCON_vec: Vector2) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.NEWCON
    ret.field_NEWCON_vec = field_NEWCON_vec
    return ret 
  
  func show() -> String:
    match self.con:
      Con.ACTION:  return "ACTION" 
      Con.BLA:  return "BLA" 
      Con.GET_STATE:  return "GET_STATE" 
      Con.GET_STATE2:  return "GET_STATE2" 
      Con.JOIN:  return "JOIN" 
      Con.LEAVE:  return "LEAVE" 
      Con.NEWCON:  return "NEWCON"  
  
  static func desArr(var arr: Array) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    match arr[0]:
      Con.ACTION:
        ret.field_ACTION_act = arr[1].desArr()
        ret.field_ACTION_integ = arr[2]
      Con.BLA:  ret.field_BLA_xxx = arr[1]
      Con.GET_STATE:  ret.field_GET_STATE_st = arr[1].desArr()
      Con.GET_STATE2:  ret.field_GET_STATE2_st2 = arr[1].desArr()
      Con.LEAVE:  ret.field_LEAVE = arr[1]
      Con.NEWCON:  ret.field_NEWCON_vec = arr[1] 
    return ret 
  
  static func des(var this: PackedByteArray) -> CliMsg:
    return desArr(bytes_to_var(bs)) 
  
  static func serArr(var this: CliMsg) -> Array:
    match this.con:
      Con.ACTION:  return  [ Con.ACTION, field_ACTION_act.serArr(), field_ACTION_integ ]  
      Con.BLA:  return  [ Con.BLA, field_BLA_xxx ]  
      Con.GET_STATE:  return  [ Con.GET_STATE, field_GET_STATE_st.serArr() ]  
      Con.GET_STATE2:  return  [ Con.GET_STATE2, field_GET_STATE2_st2.serArr() ]  
      Con.JOIN:  return  [ Con.JOIN ]  
      Con.LEAVE:  return  [ Con.LEAVE, field_LEAVE ]  
      Con.NEWCON:  return  [ Con.NEWCON, field_NEWCON_vec ]   
  
  static func ser(var this: CliMsg) -> PackedByteArray:
    return var_to_bytes(serArr(this))  