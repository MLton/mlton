(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature POPT =
   sig
      (* This type specifies what kind of arguments a switch expects
       * and provides the function to be applied to the argument.
       *)
      datatype t =
         (* one arg: a boolean (true, false), after a space *)
         Bool of bool -> unit
         (* one arg: a single digit, no space. *)
       | Digit of int -> unit
         (* one arg: an integer, after a space *)
       | Int of int -> unit
         (* one arg: an integer followed by optional k or m. *)
       | Mem of int -> unit
         (* no args *)
       | None of unit -> unit
       | Real of real -> unit
         (* Any string immediately follows the switch. *)
       | String of string -> unit
         (* one arg: any string, after a space *)
       | SpaceString of string -> unit
       | SpaceString2 of string * string -> unit
         (* one arg: a word (hex), after a space *)
       | Word of word -> unit

      val boolRef: bool ref -> t
      val falseRef: bool ref -> t
      val intRef: int ref -> t
      val stringRef: string ref -> t
      val trueRef: bool ref -> t
      val wordRef: word ref -> t

      val trace: string * t

      (* Parse the switches, applying the first matching t to each switch,
       * and return any remaining args.
       * Returns NONE if it encounters an error.
       * For example, if ts is:
       *  [("foo", None f)]
       * and the switches are:
       *  ["-foo", "bar"]
       * then parse will call f() and return "bar".
       *)
      val parse:
         {
          switches: string list,
          opts: (string * t) list
         }
         -> string list Result.t

      datatype optionStyle = Normal | Expert
      val makeUsage: {mainUsage: string,
                      makeOptions: ({usage: string -> unit}
                                    -> {style: optionStyle,
                                        name: string,
                                        arg: string,
                                        desc: string,
                                        opt: t} list),
                      showExpert: unit -> bool
                      } -> {parse: string list -> string list Result.t,
                            usage: string -> unit}
   end
