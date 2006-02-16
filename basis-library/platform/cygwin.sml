(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Cygwin =
   struct
      structure Prim = Primitive.Cygwin
         
      fun toFullWindowsPath p =
         C.CS.toString (Prim.toFullWindowsPath (NullString.nullTerm p))

      fun toExe cmd =
         let
            val cmd = toFullWindowsPath cmd
            fun addExe () = concat [cmd, ".exe"]
            fun loop i =
               let
                  val i = i - 1
               in
                  if i < 0
                     then addExe ()
                  else
                     let
                        val c = String.sub (cmd, i)
                     in
                        case c of
                           #"." => cmd
                         | #"\\" => addExe ()
                         | _ => loop i
                     end
               end
         in
            loop (size cmd)
         end
   end
            
