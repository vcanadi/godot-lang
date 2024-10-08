class Dir extends Object:

  enum Con { D, L, R, U }

  var con: Con


  #  Equality check of two Dir 
  static func eq(a: Dir, b: Dir) -> bool:
    return a.con==b.con && (a.con==Con.D || a.con==Con.L || a.con==Con.R || a.con==Con.U) 
  
  
  #  Non-static equality check of two Dir 
  func eq1(b: Dir) -> bool:
    return Dir.eq(self, b) 
  
  
  # Constructor function for sum constructor D
  static func d() -> Dir:
    var ret: Dir = Dir.new() 
    ret.con = Con.D
    return ret 
  
  
  # Constructor function for sum constructor L
  static func l() -> Dir:
    var ret: Dir = Dir.new() 
    ret.con = Con.L
    return ret 
  
  
  # Constructor function for sum constructor R
  static func r() -> Dir:
    var ret: Dir = Dir.new() 
    ret.con = Con.R
    return ret 
  
  
  # Constructor function for sum constructor U
  static func u() -> Dir:
    var ret: Dir = Dir.new() 
    ret.con = Con.U
    return ret 
  
  
  # String representation of type
  func show() -> String:
    match self.con:
      Con.D:   return "D" 
      
      Con.L:   return "L" 
      
      Con.R:   return "R" 
      
      Con.U:   return "U" 
      
      _:  return "" 
      
  
  
  # Deserialize from array
  static func desFromArr(arr: Array[Variant]) -> Dir:
    var ret: Dir = Dir.new() 
    ret.con = arr[0]
    match ret.con:
    
    return ret 
  
  
  # Serialize to array
  static func serToArr(this: Dir) -> Array[Variant]:
    match this.con:
      Con.D:   return [ Con.D ]  
      
      Con.L:   return [ Con.L ]  
      
      Con.R:   return [ Con.R ]  
      
      Con.U:   return [ Con.U ]  
      
      _:  return [] 
      
