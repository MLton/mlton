(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor LookupConstant (S: LOOKUP_CONSTANT_STRUCTS): LOOKUP_CONSTANT = 
struct

open S
local
   open Const
in
   structure IntX = IntX
   structure RealX = RealX
   structure WordX = WordX
end
structure IntSize = IntX.IntSize
structure RealSize = RealX.RealSize
structure WordSize = WordX.WordSize

val buildConstants: (string * (unit -> string)) list =
   let
      val bool = Bool.toString
      val int = Int.toString
      open Control
   in
      [("Exn_keepHistory", fn () => bool (!exnHistory)),
       ("MLton_detectOverflow", fn () => bool (!detectOverflow)),
       ("MLton_native", fn () => bool (!Native.native)),
       ("MLton_profile_isOn", fn () => bool (!profile <> ProfileNone)),
       ("MLton_safe", fn () => bool (!safe)),
       ("MLton_FFI_numExports", fn () => int (Ffi.numExports ())),
       ("TextIO_bufSize", fn () => int (!textIOBufSize))]
   end

datatype z = datatype ConstType.t

fun escape s =
   String.translate (s, fn c =>
		     let
			val i = Char.ord c
			fun dig j =
			   Char.chr
			   (Char.ord #"0" + Int.rem (Int.quot (i, j), 10))
		     in
			implode [dig 100, dig 10, dig 1]
		     end)
   
fun unescape s =
   let
      fun sub i = Char.toInt (String.sub (s, i)) - Char.toInt #"0"
      fun loop (i, ac) =
	 if i < 0
	    then ac
	 else
	    loop (i - 3,
		  Char.fromInt ((sub (i - 2) * 10 + sub (i - 1)) * 10 + sub i)
		  :: ac)
   in
      implode (loop (String.size s - 1, []))
   end

val unescape = Trace.trace ("unescape", String.layout, String.layout) unescape

val gcFields =
   [
    "canHandle",
    "currentThread",
    "exnStack",
    "frontier",
    "cardMapForMutator",
    "limit",
    "limitPlusSlop",
    "maxFrameSize",
    "signalIsPending",
    "stackBottom",
    "stackLimit",
    "stackTop"
    ]

val gcFields =
   List.map (gcFields, fn s =>
	     {name = s,
	      value = concat ["offsetof (struct GC_state, ", s, ")"],
	      ty = ConstType.Int})

fun build (constants, out) =
   let
      val constants =
	 List.fold
	 (constants, gcFields, fn ((name, ty), ac) =>
	  if List.exists (buildConstants, fn (name', _) => name = name')
	     then ac
	  else {name = name, value = name, ty = ty} :: ac)
   in
      List.foreach
      (List.concat
       [["#include <stddef.h>", (* for offsetof *)
	 "#include <stdio.h>"],
	List.map (["libmlton.h"], fn i =>
		  concat ["#include <", i, ">"]),
	["struct GC_state gcState;",
	 "int main (int argc, char **argv) {"],
	List.revMap
	(constants, fn {name, value, ty} =>
	 let
	    val (format, value) =
	       case ty of
		  Bool => ("%s", concat [value, "? \"true\" : \"false\""])
		| Int => ("%d", value)
		| Real => ("%.20f", value)
		| String => ("%s", concat ["\"", escape value, "\""])
		| Word => ("%u", value)
	 in
	    concat ["fprintf (stdout, \"", name, " = ", format, "\\n\", ",
		    value, ");"]
	 end),
	["return 0;}"]],
       fn l => (Out.output (out, l); Out.newline out))
   end

fun load (ins: In.t): string * ConstType.t -> Const.t =
   let
      val table: {hash: word, name: string, value: string} HashSet.t =
	 HashSet.new {hash = #hash}
      fun add {name, value} =
	 let
	    val hash = String.hash name
	    val _ = 
	       HashSet.lookupOrInsert
	       (table, hash,
		fn {name = name', ...} => name = name',
		fn () => {hash = hash, name = name, value = value})
	 in
	    ()
	 end
      val buildConstants =
	 List.foreach (buildConstants, fn (name, f) =>
		       add {name = name, value = f ()})
      val _ = 
	 In.foreachLine
	 (ins, fn l =>
	  case String.tokens (l, Char.isSpace) of
	     [name, "=", value] => add {name = name, value = value}
	   | _ => Error.bug (concat ["strange constants line: ", l]))
      fun lookupConstant (name: string, ty: ConstType.t): Const.t =
	 let
 	    val {value, ...} =
 	       HashSet.lookupOrInsert
 	       (table, String.hash name,
 		fn {name = name', ...} => name = name',
 		fn () => Error.bug (concat ["constant not found: ", name]))
	    fun int i = Const.int (IntX.make (i, IntSize.default))
	 in
	    case ty of
	       Bool =>
		  (case Bool.fromString value of
		      NONE => Error.bug "strange Bool constant"
		    | SOME b => int (if b then 1 else 0))
	     | Int => 
		  (case IntInf.fromString value of
		      NONE => Error.bug "strange Int constant"
		    | SOME i => int i)
	     | Real =>
		  (case RealX.make (value, RealSize.default) of
		      NONE => Error.bug "strange Real constant"
		    | SOME r => Const.Real r)
	     | String => Const.string (unescape value)
	     | Word =>
		  (case IntInf.fromString value of
		      NONE => Error.bug "strange Word constant"
		    | SOME i =>
			 Const.Word (WordX.make (LargeWord.fromIntInf i,
						 WordSize.default)))
	 end
   in
      lookupConstant
   end

end
