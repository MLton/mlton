(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Exn:> EXN =
struct

type t = exn

val history = MLton.Exn.history
   
val name = General.exnName

exception Bind = Bind
exception Match = Match
exception Overflow = Overflow
exception Subscript = Subscript
   
fun layout e =
   let open Layout
   in case e of
      OS.SysErr(s, so) =>
	 seq[str "error: ",
	     case so of
		NONE => empty
	      | SOME se => seq[str(OS.errorName se), str ": "],
             str s]
    | Fail s => str s
    | _ => seq[str "unhandled exception: ", str(exnName e)]
   end

end
   
