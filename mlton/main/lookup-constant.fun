(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
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
   structure RealX = RealX
   structure WordX = WordX
end
structure RealSize = RealX.RealSize
structure WordSize = WordX.WordSize

val buildConstants: (string * (unit -> string)) list =
   let
      val bool = Bool.toString
      val int = Int.toString
      open Control
   in
      [("MLton_codegen", fn () => int (case !codegen of
					  Bytecode => 0
					| CCodegen => 1
					| CmmCodegen => 2
					| Native => 3)),
       ("MLton_profile_isOn", fn () => bool (case !profile of
						ProfileNone => false
					      | ProfileCallStack => false
					      | ProfileMark => false
					      | _ => true)),
       ("MLton_FFI_numExports", fn () => int (Ffi.numExports ()))]
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
	      ty = ConstType.Word})

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
       [["#define _ISOC99_SOURCE",
	 "#define _POSIX_C_SOURCE 200112L",
	 "",
	 "#include \"platform.h\"",
	 "struct GC_state gcState;",
	 "",
	 "int main (int argc, char **argv) {"],
	List.revMap
	(constants, fn {name, value, ty} =>
	 let
	    val (format, value) =
	       case ty of
		  Bool => ("%s", concat [value, "? \"true\" : \"false\""])
		| Real => ("%.20f", value)
		| String => ("%s", value)
		| Word => ("%u", value)
	 in
	    concat ["fprintf (stdout, \"", name, " = ", format, "\\n\", ",
		    value, ");"]
	 end),
	["return 0;}"]],
       fn l => (Out.output (out, l); Out.newline out))
   end

fun load (ins: In.t, commandLineConstants)
   : {default: string option, name: string} * ConstType.t -> Const.t =
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
      val () =
	 List.foreach (buildConstants, fn (name, f) =>
		       add {name = name, value = f ()})
      val () =
	 List.foreach
	 (commandLineConstants, fn {name, value} =>
	  let
	  in
	     add {name = name, value = value}
	  end)
      val _ = 
	 In.foreachLine
	 (ins, fn l =>
	  case String.tokens (l, Char.isSpace) of
	     [name, "=", value] => add {name = name, value = value}
	   | _ => Error.bug (concat ["strange constants line: ", l]))
      fun lookupConstant ({default, name}, ty: ConstType.t): Const.t =
	 let
 	    val {value, ...} =
	       let
		  val hash = String.hash name
	       in
		  HashSet.lookupOrInsert
		  (table, hash,
		   fn {name = name', ...} => name = name',
		   fn () =>
		   case default of
		      NONE => Error.bug (concat ["constant not found: ", name])
		    | SOME value =>
			 {hash = hash,
			  name = name,
			  value = value})
	       end
	    fun error (t: string) =
	       Error.bug (concat ["constant ", name, " expects a ", t,
				  " but got ", value, "."])
	 in
	    case ty of
	       Bool =>
		  (case Bool.fromString value of
		      NONE => error "bool"
		    | SOME b =>
			 Const.Word (WordX.fromIntInf
				     (if b then 1 else 0, WordSize.default)))
	     | Real =>
		  (case RealX.make (value, RealSize.default) of
		      NONE => error "real"
		    | SOME r => Const.Real r)
	     | String => Const.string value
	     | Word =>
		  (case IntInf.fromString value of
		      NONE => error "int"
		    | SOME i =>
			 Const.Word (WordX.fromIntInf (i, WordSize.default)))
	 end
   in
      lookupConstant
   end

end
