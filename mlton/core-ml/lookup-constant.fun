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
open CoreML

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

structure ConstType =
   struct
      datatype t = Int | Real | String | Word
   end
datatype z = datatype ConstType.t

type res = (string * ConstType.t) list

fun decsConstants (decs: Dec.t vector): res =
   let
      datatype z = datatype Exp.node
      datatype z = datatype Dec.node
      fun loopExp (e: Exp.t, ac: res): res =
	 case Exp.node e of
	    App (e, e') => loopExp (e, loopExp (e', ac))
	  | Constraint (e, _) => loopExp (e, ac)
	  | Fn {match = m, ...} => loopMatch (m, ac)
	  | Handle (e, m) => loopMatch (m, loopExp (e, ac))
	  | Let (ds, e) => loopDecs (ds, loopExp (e, ac))
	  | Prim p =>
	       (case Prim.name p of
		   Prim.Name.Constant c =>
		      let
			 fun strange () =
			    Error.bug
			    (concat ["constant with strange type: ", c])
			 val Scheme.T {tyvars, ty} = Prim.scheme p
		      in
			 if 0 = Vector.length tyvars
			    then
			       let
				  val tys =
				     [(Type.defaultInt, Int),
				      (Type.defaultReal, Real),
				      (Type.word8Vector, String),
				      (Type.defaultWord, Word)]
			       in case (List.peek
					(tys, fn (ty', _) =>
					 Type.equals (ty, ty'))) of
				  NONE => strange ()
				| SOME (_, t) => (c, t) :: ac

			       end
			 else strange ()
		      end
		 | _ => ac)
	  | Raise {exn, ...} => loopExp (exn, ac)
	  | Record r => Record.fold (r, ac, loopExp)
	  | _ => ac
      and loopMatch (m, ac: res): res =
	 Vector.fold (Match.rules m , ac, fn ((_, e), ac) => loopExp (e, ac))
      and loopDecs (ds: Dec.t vector, ac: res): res =
	 Vector.fold (ds, ac, loopDec)
      and loopDec (d: Dec.t, ac: res): res =
	 case Dec.node d of
	    Val {exp, ...} => loopExp (exp, ac)
	  | Fun {decs, ...} =>
	       Vector.fold (decs, ac, fn ({match, ...}, ac) =>
			    loopMatch (match, ac))
	  | _ => ac
   in
      loopDecs (decs, [])
   end

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

fun constants decs =
   List.append
   (List.map (decsConstants decs, fn (s, t) =>
	      {name = s,
	       value = s,
	       ty = t}),
    List.map (gcFields, fn s =>
	      {name = s,
	       value = concat ["offsetof (struct GC_state, ", s, ")"],
	       ty = ConstType.Int}))
	       
fun build (decs: CoreML.Dec.t vector, out: Out.t): unit =
   List.foreach
   (List.concat
    [["#include <stddef.h>", (* for offsetof *)
      "#include <stdio.h>"],
     List.map (["libmlton.h"], fn i =>
	       concat ["#include <", i, ">"]),
     ["struct GC_state gcState;",
      "int main (int argc, char **argv) {"],
     List.map
     (constants decs, fn {name, value, ty} =>
      let
	 fun doit (format, value) =
	    concat ["fprintf (stdout, \"", name, " = ", format, "\\n\", ",
		    value, ");"]
      in
	 case ty of
	    Int => doit ("%d", value)
	  | Real => doit ("%.20f", value)
	  | String => concat ["MLton_printStringEscaped (f, ", value, ");"]
	  | Word => doit ("%u", value)
      end),
     ["return 0;}"]],
    fn l => (Out.output (out, l); Out.newline out))

fun load (decs, ins: In.t): string -> Const.t =
   let
      val constants = constants decs
      val lines = In.lines ins
      val _ = if List.length constants = List.length lines
		 then ()
	      else Error.bug "bad constants file"
      val values =
	 List.map2
	  (constants, lines,
	   fn ({name, ty, ...}, s) =>
	   let
	      val s =
		 case String.tokens (s, Char.isSpace) of
		    [name', "=", value] =>
		       if name = name'
			  then value
		       else Error.bug (concat ["expected ", name,
					       " but saw ", name'])
		  | _ => Error.bug (concat ["strange constants line ", s])
	   in
	      case ty of
		 Int =>
		    (case IntInf.fromString s of
			NONE => Error.bug "strange Int constant"
		      | SOME i => 
			   Const.Int (IntX.make (i, IntSize.default)))
	       | String => Const.string (unescape s)
	       | Real => Const.Real (RealX.make (s, RealSize.default))
	       | Word =>
		    (case IntInf.fromString s of
			NONE => Error.bug "strange Word constant"
		      | SOME i => 
			   Const.Word (WordX.fromLargeInt (i, WordSize.default)))
	   end)
      val lookupConstant =
	 String.memoizeList
	 (fn s => Error.bug (concat ["strange constant: ", s]),
	  List.fold2 (constants, values, [],
		      fn ({name, ...}, v, ac) => (name, v) :: ac))
   in
      lookupConstant      
   end

end
