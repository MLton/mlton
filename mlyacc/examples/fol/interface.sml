(* Externally visible aspects of the lexer and parser 
 *
 * $Log: interface.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:03  george
 *   Version 109.24
 *
 * Revision 1.1.1.1  1996/01/31  16:01:39  george
 * Version 109
 * 
 *)

signature INTERFACE =
sig

type pos
val line : pos ref
val init_line : unit -> unit
val next_line : unit -> unit
val error : string * pos * pos -> unit

type arg
val nothing : arg

end  (* signature INTERFACE *)

functor Interface () : INTERFACE =
struct

type pos = int
val line = ref 0
fun init_line () = (line := 1)
fun next_line () = (line := !line + 1)
fun error (errmsg,line:pos,_) =
  output(std_out,"Line " ^ (makestring line) ^ ": " ^ errmsg ^ "\n")

type arg = unit

val nothing = ()

end  (* functor INTERFACE *)
