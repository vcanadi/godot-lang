class_name CliMsg extends object

  enum Con { ACTION, BLA, GET_STATE, JOIN, LEAVE, NEWCON }

  var field_ACTION_act: Action
  var field_ACTION_integ: int
  var field_BLA_xxx: String
  var field_LEAVE: float
  var field_NEWCON_vec: Vector2

  static func action(var field_ACTION_act: Action, var field_ACTION_integ: int) -> CliMsg:
    var this: CliMsg = CliMsg.new() 
    this.con = Con.ACTION
    this.field_ACTION_act = field_ACTION_act
    this.field_ACTION_integ = field_ACTION_integ
    return this 
  
  static func bla(var field_BLA_xxx: String) -> CliMsg:
    var this: CliMsg = CliMsg.new() 
    this.con = Con.BLA
    this.field_BLA_xxx = field_BLA_xxx
    return this 
  
  static func get_state() -> CliMsg:
    var this: CliMsg = CliMsg.new() 
    this.con = Con.GET_STATE
    return this 
  
  static func join() -> CliMsg:
    var this: CliMsg = CliMsg.new() 
    this.con = Con.JOIN
    return this 
  
  static func leave(var field_LEAVE: float) -> CliMsg:
    var this: CliMsg = CliMsg.new() 
    this.con = Con.LEAVE
    this.field_LEAVE = field_LEAVE
    return this 
  
  static func newcon(var field_NEWCON_vec: Vector2) -> CliMsg:
    var this: CliMsg = CliMsg.new() 
    this.con = Con.NEWCON
    this.field_NEWCON_vec = field_NEWCON_vec
    return this 
  
  func show() -> String:
    match self.con:
      Con.ACTION: return "ACTION" 
      Con.BLA: return "BLA" 
      Con.GET_STATE: return "GET_STATE" 
      Con.JOIN: return "JOIN" 
      Con.LEAVE: return "LEAVE" 
      Con.NEWCON: return "NEWCON"  
  
  static func serArr(var this: CliMsg) -> Array:
    match this.con:
      Con.ACTION: return  [ Con.ACTION, field_ACTION_act.serArr(), field_ACTION_integ ]  
      Con.BLA: return  [ Con.BLA, field_BLA_xxx ]  
      Con.GET_STATE: return  [ Con.GET_STATE ]  
      Con.JOIN: return  [ Con.JOIN ]  
      Con.LEAVE: return  [ Con.LEAVE, field_LEAVE ]  
      Con.NEWCON: return  [ Con.NEWCON, field_NEWCON_vec ]   
  
  static func ser(var this: CliMsg) -> PackedByteArray:
    return var_to_bytes(serArr(this))  