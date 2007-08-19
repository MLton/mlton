(* Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
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
         end handle e as PosixError.SysErr (_, SOME s) =>
            if s = Posix.Error.exist
               then loop ()
            else raise e
   in
      loop ()
   end

fun mkstemp s = mkstemps {prefix = s, suffix = ""}

fun tempPrefix file =
   case MLtonPlatform.OS.host of
      MLtonPlatform.OS.MinGW =>
      (case MinGW.getTempPath () of
          SOME d => d
        | NONE => "C:\\temp\\") ^ file
    | _ => "/tmp/" ^ file

end
