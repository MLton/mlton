(* Modified from SML/NJ sources by sweeks@research.nj.nec.com on 4/18/98. *)
(* Modified by fluet@cs.cornell.edu on 7/19/02. *)

(* option.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *)

structure Option: OPTION =
  struct
     datatype 'a option = NONE | SOME of 'a
	
     exception Option

     val getOpt =
	fn (SOME x, y) => x
	 | (NONE, y) => y
	 
     val isSome =
	fn SOME _ => true
	 | NONE => false
	 
     val valOf =
	fn SOME x => x
	 | _ => raise Option
	 
     fun filter pred x = if (pred x) then SOME x else NONE

     val join =
	fn SOME opt => opt
	 | NONE => NONE

     fun app f =
        fn SOME x => f x
	 | NONE => ()

     fun map f =
	fn SOME x => SOME (f x)
	 | NONE => NONE

     fun mapPartial f =
	fn SOME x => f x
	 | NONE => NONE

     fun compose (f, g) x = map f (g x)

     fun composePartial (f, g) x = mapPartial f (g x)
  end

structure OptionGlobal: OPTION_GLOBAL = Option
open OptionGlobal
