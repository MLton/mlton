(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t

signature POPT =
   sig
      (* This type specifies what kind of arguments a switch expects
       * and provides the function to be applied to the argument.
       *)
      datatype opt =
	 (* no args *)
	 None of unit -> unit
	 (* one arg: an integer, after a space *)
       | Int of int -> unit
         (* one arg: a boolean (true, false), after a space *)
       | Bool of bool -> unit
	 (* one arg: a single digit, no space. *)
       | Digit of int -> unit
	 (* one arg: an integer followed by optional k or m. *)
       | Mem of int -> unit
	 (* Any string immediately follows the switch. *)
       | String of string -> unit
	 (* one arg: any string *)
       | SpaceString of string -> unit
       | SpaceString2 of string * string -> unit

      val boolRef: bool ref -> opt
      val falseRef: bool ref -> opt
      val intRef: int ref -> opt
      val stringRef: string ref -> opt
      val trueRef: bool ref -> opt

      val trace: string * opt
	 
      (* Parse the switches, applying the first matching opt to each switch,
       * and return any remaining args.
       * Returns NONE if it encounters an error.
       * For example, if opts is:
       *  [("foo", None f)]
       * and the switches are:
       *  ["-foo", "bar"]
       * then parse will call f() and return "bar".
       *)
      val parse:
	 {
	  switches: string list,
	  opts: (string * opt) list
	 }
	 -> string list Result.t
   end
