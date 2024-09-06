class_name CliMsg extends object

  enum Con { ACTION, GET_LISTS, GET_MAP, JOIN, LEAVE }

  var fld_ACTION_act: Action
  var fld_ACTION_time: int
  var fld_ACTION_vec: Vector2
  var fld_GET_LISTS_glList: Array[Array[float]]
  var fld_GET_LISTS_glList2: Array[Action]
  var fld_GET_LISTS_glList3: Array[Array[Array[Action]]]
  var fld_GET_MAP_gmMap: Dictionary[float, String]
  var fld_LEAVE_0: String

  # Constructor function for sum constructor ACTION
  static func action(var fld_ACTION_act: Action, var fld_ACTION_time: int, var fld_ACTION_vec: Vector2) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.ACTION
    ret.fld_ACTION_act = fld_ACTION_act
    ret.fld_ACTION_time = fld_ACTION_time
    ret.fld_ACTION_vec = fld_ACTION_vec
    return ret 
  
  # Constructor function for sum constructor GET_LISTS
  static func get_lists(var fld_GET_LISTS_glList: Array[Array[float]], var fld_GET_LISTS_glList2: Array[Action], var fld_GET_LISTS_glList3: Array[Array[Array[Action]]]) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.GET_LISTS
    ret.fld_GET_LISTS_glList = fld_GET_LISTS_glList
    ret.fld_GET_LISTS_glList2 = fld_GET_LISTS_glList2
    ret.fld_GET_LISTS_glList3 = fld_GET_LISTS_glList3
    return ret 
  
  # Constructor function for sum constructor GET_MAP
  static func get_map(var fld_GET_MAP_gmMap: Dictionary[float, String]) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.GET_MAP
    ret.fld_GET_MAP_gmMap = fld_GET_MAP_gmMap
    return ret 
  
  # Constructor function for sum constructor JOIN
  static func join() -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.JOIN
    return ret 
  
  # Constructor function for sum constructor LEAVE
  static func leave(var fld_LEAVE_0: String) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = Con.LEAVE
    ret.fld_LEAVE_0 = fld_LEAVE_0
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
  static func desFromArr(var arr: Array[Variant]) -> CliMsg:
    var ret: CliMsg = CliMsg.new() 
    ret.con = arr[0]
    match arr[0]:
      Con.ACTION:
        ret.fld_ACTION_act = arr[1].desFromArr()
        ret.fld_ACTION_time = arr[2]
        ret.fld_ACTION_vec = arr[3]
      Con.GET_LISTS:
        ret.fld_GET_LISTS_glList.assign(arr[1].map(func(x): x))
        ret.fld_GET_LISTS_glList2.assign(arr[2].map(desFromArr))
        ret.fld_GET_LISTS_glList3.assign(arr[3].map(func(x): x.map(func(x): x.map(desFromArr))))
      Con.GET_MAP:  ret.fld_GET_MAP_gmMap = arr[1]
      Con.LEAVE:  ret.fld_LEAVE_0 = arr[1] 
    return ret 
  
  # Deserialize from binary
  static func des(var this: PackedByteArray) -> CliMsg:
    return desFromArr(bytes_to_var(bs)) 
  
  # Serialize to array
  static func serToArr(var this: CliMsg) -> Array[Variant]:
    match this.con:
      Con.ACTION:  return  [ Con.ACTION, fld_ACTION_act.serToArr(), fld_ACTION_time, fld_ACTION_vec ]  
      Con.GET_LISTS:  return  [ Con.GET_LISTS, fld_GET_LISTS_glList.map(func(x): x), fld_GET_LISTS_glList2.map(serToArr), fld_GET_LISTS_glList3.map(func(x): x.map(func(x): x.map(serToArr))) ]  
      Con.GET_MAP:  return  [ Con.GET_MAP, fld_GET_MAP_gmMap ]  
      Con.JOIN:  return  [ Con.JOIN ]  
      Con.LEAVE:  return  [ Con.LEAVE, fld_LEAVE_0 ]   
  
  # Serialize to binary
  static func ser(var this: CliMsg) -> PackedByteArray:
    return var_to_bytes(serToArr(this))  