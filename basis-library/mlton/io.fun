(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MLtonIO (S: MLTON_IO_ARG): MLTON_IO =
struct

open S

fun mkstemps {prefix, suffix}: string * outstream =
   let
      fun loop () =
         let
            val name = concat [prefix, MLtonRandom.alphaNumString 6, suffix]
            open Posix.FileSys
         in
            (name,
             newOut (createf (name, O_WRONLY, O.flags [O.excl],
                              let open S
                              in flags [irusr, iwusr]
                              end),
                     name))
         end handle e as PosixError.SysErr (_, s) =>
            if s = SOME Posix.Error.exist
               then loop ()
            else raise e
   in
      loop ()
   end

fun mkstemp s = mkstemps {prefix = s, suffix = ""}

local
   fun tmpdir () =
      case OS.Process.getEnv "TMPDIR" of
         NONE => NONE
       | SOME d =>
         if OS.Path.isAbsolute d andalso OS.Path.isCanonical d
            andalso OS.FileSys.isDir d
            then SOME d
         else NONE
in
   fun tempPrefix file =
      case tmpdir () of
         SOME d => OS.Path.joinDirFile {dir = d, file = file}
       | NONE =>
         case MLtonPlatform.OS.host of
            MLtonPlatform.OS.MinGW =>
            (case MinGW.getTempPath () of
                SOME d => d
              | NONE => "C:\\temp\\") ^ file
          | _ => "/tmp/" ^ file
end

end
