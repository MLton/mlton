(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t

structure Popt: POPT =
struct

datatype opt =
   None of unit -> unit
 | Int of int -> unit
 | Bool of bool -> unit
 | Digit of int -> unit
 | Mem of int -> unit
 | String of string -> unit
 | SpaceString of string -> unit
 | SpaceString2 of string * string -> unit

local
   fun make b (r: bool ref): opt = None (fn () => r := b)
in
   val trueRef = make true
   val falseRef = make false
end

fun boolRef (r: bool ref): opt = Bool (fn b => r := b)
   
fun intRef (r: int ref): opt = Int (fn n => r := n)

fun stringRef (r: string ref): opt = String (fn s => r := s)

val trace = ("trace", SpaceString (fn s =>
				   let open Trace.Immediate
				      val _ = debug := Out Out.error
				   in case s of
				      "*" => always ()
				    | _ => (flagged ()
					    ; on (String.split (s, #",")))
				   end))

fun memString (s: string): int option =
   let
      val n = String.size s
      fun loop (i, ac) =
	 if i = n
	    then SOME ac
	 else
	    let val c = String.sub (s, i)
	       fun done n = SOME (n * ac)
	    in case c of
	       #"m" => done 1048576
	     | #"k" => done 1024
	     | _ =>
		  case Char.digitToInt c of
		     NONE => NONE
		   | SOME c => loop (i + 1, ac * 10 + c)
	    end
   in loop (0, 0)
   end

(* Parse the command line opts and return any remaining args. *)
fun parse {switches: string list,
	   opts: (string * opt) list}: string list Result.t =
   let
      exception Error of string
      val rec loop =
	 fn [] => []
	  | switch :: switches =>
	       let
		  fun error () = raise (Error switch)
	       in
		  case String.sub (switch, 0) of
		     #"-" =>
		     let val switch = String.dropPrefix (switch, 1)
		     in case List.peek (opts, fn (switch', arg) =>
					switch = switch') of
			NONE =>
			   let
			      val rec loop' =
				 fn [] => error ()
				  | (switch', arg) :: opts =>
				       let
					  fun doit f =
					     if String.isPrefix
						{string = switch, prefix = switch'}
						then f (String.dropPrefix
							(switch, String.size switch'))
					     else loop' opts
				       in case arg of
					  Digit f =>
					     doit (fn s =>
						   if size s = 1
						      then (case Char.digitToInt
							       (String.sub (s, 0)) of
							       NONE => error ()
							     | SOME i => f i)
						   else error ())
					| String f => doit f
					| _ => loop' opts
				       end
			   in loop' opts
			      ; loop switches
			   end
		      | SOME (_, arg) =>
			   let
			      fun next (f, get) =
				 case switches of
				    switch :: switches =>
				       (case get switch of
					   SOME n => (f n; loop switches)
					 | _ => error ())
				  | _ => error ()
			   in case arg of
			      None f => (f (); loop switches)
			    | Int f => next (f, Int.fromString)
			    | Bool f => next (f, Bool.fromString)
			    | SpaceString f => next (f, SOME)
			    | SpaceString2 f =>
				 (case switches of
				     s1 :: s2 :: switches =>
					(f (s1, s2); loop switches)
				   | _ => error ())
			    | String f => (f ""; loop switches)
			    | Mem f => next (f, memString)
			    | _ => error ()
			   end
		     end
		   | _ => switch :: switches
	       end
   in
      Result.Yes (loop switches) handle Error s => Result.No s
   end
end
