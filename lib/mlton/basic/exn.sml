(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
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
   let
      open Layout
   in
      case e of
	 OS.SysErr (s, _) => str s
       | Fail s => str s
       | IO.Io {cause, function, name, ...} =>
	    seq [str (concat [function, " ", name, ": "]), layout cause]
       | _ => seq [str "unhandled exception: ", str (exnName e)]
   end

val toString = Layout.toString o layout

local
   (* would like to make the declaration of z in a let inside the try function,
    * with 'a as a free type variable.  But SML/NJ doesn't allow it.
    *)
   datatype 'a z = Ok of 'a | Raise of exn
in
   val try: (unit -> 'a) * ('a -> 'b) * (exn -> 'b) -> 'b =
      fn (t, k, h) =>
      case Ok (t ()) handle e => Raise e of
	 Ok x => k x
       | Raise e => h e
end

end
