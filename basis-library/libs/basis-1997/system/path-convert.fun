functor OSPathConvert
        (structure Path : OS_PATH) :
        OS_PATH_1997 =
  struct
     open Path

     val mkAbsolute = fn (path, relativeTo) =>
       mkAbsolute {path = path, relativeTo = relativeTo}
     val mkRelative = fn (path, relativeTo) =>
       mkRelative {path = path, relativeTo = relativeTo}
  end