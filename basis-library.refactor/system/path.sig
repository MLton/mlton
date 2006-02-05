signature OS_PATH =
   sig
      exception InvalidArc
      exception Path

      val base: string -> string
      val concat: string * string -> string
      val currentArc: string
      val dir: string -> string
      val ext: string -> string option
      val file: string -> string
      val fromString: string -> {isAbs: bool, vol: string, arcs: string list}
      val fromUnixPath: string -> string
      val getParent: string -> string
      val getVolume: string -> string
      val isAbsolute: string -> bool
      val isCanonical: string -> bool
      val isRelative: string -> bool
      val isRoot: string -> bool
      val joinBaseExt: {base: string, ext: string option} -> string
      val joinDirFile: {dir: string, file: string} -> string
      val mkAbsolute: {path: string, relativeTo: string} -> string
      val mkCanonical: string -> string
      val mkRelative: {path: string, relativeTo: string} -> string
      val parentArc: string
      val splitBaseExt: string -> {base: string, ext: string option}
      val splitDirFile: string -> {dir: string, file: string}
      val toString: {isAbs: bool, vol: string, arcs: string list} -> string
      val toUnixPath: string -> string
      val validVolume: {isAbs: bool, vol: string} -> bool
   end
