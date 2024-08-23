class_name CliMsg extends object

  enum Con { ACTION, GET_LISTS, GET_MAP, JOIN, LEAVE }

  var field_ACTION_act: Action
  var field_ACTION_time: int
  var field_ACTION_vec: Vector2
  var field_GET_LISTS_glList: Array
  var field_GET_LISTS_glList2: Array
  var field_GET_MAP_gmMap: Dictionary
  var field_LEAVE: String

  # Constructor function for sum constructor ACTION
  static func action(var field_ACTION_act: Action, var field_ACTION_time: int, var field_ACTION_vec: Vector2) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.ACTION
    ret.field_ACTION_act = field_ACTION_act
    ret.field_ACTION_time = field_ACTION_time
    ret.field_ACTION_vec = field_ACTION_vec
    return ret 
  
  # Constructor function for sum constructor GET_LISTS
  static func get_lists(var field_GET_LISTS_glList: Array, var field_GET_LISTS_glList2: Array) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.GET_LISTS
    ret.field_GET_LISTS_glList = field_GET_LISTS_glList
    ret.field_GET_LISTS_glList2 = field_GET_LISTS_glList2
    return ret 
  
  # Constructor function for sum constructor GET_MAP
  static func get_map(var field_GET_MAP_gmMap: Dictionary) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.GET_MAP
    ret.field_GET_MAP_gmMap = field_GET_MAP_gmMap
    return ret 
  
  # Constructor function for sum constructor JOIN
  static func join() -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.JOIN
    return ret 
  
  # Constructor function for sum constructor LEAVE
  static func leave(var field_LEAVE: String) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.LEAVE
    ret.field_LEAVE = field_LEAVE
    return ret 
  
  # String representation of type
  func show() -> String:
    match self.con:
      Con.ACTION:  return "ACTION" 
      Con.GET_LISTS:  return "GET_LISTS" 
      Con.GET_MAP:  return "GET_MAP" 
      Con.JOIN:  return "JOIN" 
      Con.LEAVE:  return "LEAVE"  
  
  # Deserialize from array
  static func desFromArr(var arr: Array) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    match arr[0]:
      Con.ACTION:
        ret.field_ACTION_act = arr[1].desFromArr()
        ret.field_ACTION_time = arr[2]
        ret.field_ACTION_vec = arr[3]
      Con.GET_LISTS:
        ret.field_GET_LISTS_glList = arr[1].desFromArr()
        ret.field_GET_LISTS_glList2 = arr[2].desFromArr()
      Con.GET_MAP:  ret.field_GET_MAP_gmMap = arr[1].desFromArr()
      Con.LEAVE:  ret.field_LEAVE = arr[1] 
    return ret 
  
  # Deserialize from binary
  static func des(var this: PackedByteArray) -> CliMsg:
    return desFromArr(bytes_to_var(bs)) 
  
  # Serialize to array
  static func serToArr(var this: CliMsg) -> Array:
    match this.con:
      Con.ACTION:  return  [ Con.ACTION, field_ACTION_act.serToArr(), field_ACTION_time, field_ACTION_vec ]  
      Con.GET_LISTS:  return  [ Con.GET_LISTS, field_GET_LISTS_glList.map(func(x): x), field_GET_LISTS_glList2.map(func(x): Action.serToArr()) ]  
      Con.GET_MAP:  return  [ Con.GET_MAP, field_GET_MAP_gmMap.serToArr() ]  
      Con.JOIN:  return  [ Con.JOIN ]  
      Con.LEAVE:  return  [ Con.LEAVE, field_LEAVE ]   
  
  # ser
  static func Serialize to binary(var this: CliMsg) -> PackedByteArray:
    return var_to_bytes(serToArr(this))  