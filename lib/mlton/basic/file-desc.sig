signature FILE_DESC =
   sig
      type t = Posix.FileSys.file_desc

      val close: t -> unit
      val dup: t -> t
      val dup2: {old: t, new: t} -> unit
      val fluidLet: t * t * (unit -> 'a) -> 'a
      val move: {from: t, to: t} -> unit
      val pipe: unit -> {infd: t, outfd: t}
      val stderr: t
      val stdin: t
      val stdout: t
   end
