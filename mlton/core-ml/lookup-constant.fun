functor LookupConstant (S: LOOKUP_CONSTANT_STRUCTS): LOOKUP_CONSTANT = 
struct

open S

structure Int =
   struct
      open Int
      val fromString =
	 Trace.trace ("Int.fromString", String.layout, Option.layout layout)
	 fromString
   end

structure Word =
   struct
      open Word
      val fromString =
	 Trace.trace ("Word.fromString", String.layout, Option.layout layout)
	 fromString
   end

structure Const =
   struct
      datatype t =
	 Bool of bool
       | Int of int
       | Real of string
       | String of string
       | Word of word
   end

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
   in implode (loop (String.size s - 1, []))
   end

val unescape = Trace.trace ("unescape", String.layout, String.layout) unescape

fun build (CoreML.Program.T {decs, ...},
	   docc: {input: File.t, output: File.t} -> unit): string -> Const.t =
   let
      datatype constantType = Bool | Int | Real | String | Word
      local
	 open CoreML
	 open Exp Dec

	 type ac = (string * constantType) list
	 fun loopExp (e: Exp.t, ac: ac): ac =
	    case e of
	       Prim p =>
		  (case Prim.name p of
		      Prim.Name.Constant c =>
			 let
			    fun strange () =
			       Error.bug
			       (concat ["constant with strange type: ", c])
			 in case Prim.scheme p of
			    Scheme.T {tyvars, ty = Type.Con (tc, ts)} =>
			       if 0 = Vector.length ts
				  andalso 0 = Vector.length tyvars
				  then 
				     let
					val tycons = [(Tycon.bool, Bool),
						      (Tycon.int, Int),
						      (Tycon.real, Real),
						      (Tycon.string, String),
						      (Tycon.word, Word)]
				     in case (List.peek
					      (tycons, fn (tc', _) =>
					       Tycon.equals (tc, tc'))) of
					NONE => strange ()
				      | SOME (_, t) => (c, t) :: ac
				     end
			       else strange ()
			  | _ => strange ()
			 end
		    | _ => ac)
	     | Record r => Record.fold (r, ac, loopExp)
	     | Fn m => loopMatch (m, ac)
	     | App (e, e') => loopExp (e, loopExp (e', ac))
	     | Let (ds, e) => loopDecs (ds, loopExp (e, ac))
	     | Constraint (e, _) => loopExp (e, ac)
	     | Handle (e, m) => loopMatch (m, loopExp (e, ac))
	     | Raise {exn, ...} => loopExp (exn, ac)
	     | _ => ac
	 and loopMatch (Match.T {rules,...}, ac: ac): ac =
	    Vector.fold (rules, ac, fn ((_, e), ac) => loopExp (e, ac))
	 and loopDecs (ds: Dec.t vector, ac: ac): ac =
	    Vector.fold (ds, ac, loopDec)
	 and loopDec (d: Dec.t, ac: ac): ac =
	    case d of
	       Val {exp, ...} => loopExp (exp, ac)
	     | Fun {decs, ...} =>
		  Vector.fold (decs, ac, fn ({match, ...}, ac) =>
			       loopMatch (match, ac))
	     | _ => ac
      in
	 val constants = loopDecs (decs, [])
      end

      fun writeC out =
	 List.foreach
	 (List.concat
	  [["#include <stdio.h>"],
	   List.map (!Control.includes, fn i =>
		     concat ["#include <", i, ">"]),
	   ["struct GC_state gcState;",
	    "int main (int argc, char **argv) {",
	    "FILE *f = fopen (argv [1], \"w\");"],
	   List.map
	   (constants, fn (s, t) =>
	    let
	       fun doit (format, value) =
		  concat ["fprintf (f, \"", format, "\\n\", ",
			  value, ");"]
	    in case t of
	       Bool => doit ("%s", concat [s, "? \"true\" : \"false\""])
	     | Int => doit ("%d", s)
	     | Real => doit ("%.20f", s)
	     | String =>
		  concat ["MLton_printStringEscaped (f, ", s, ");"]
	     | Word => doit ("%x", s)
	    end),
	   ["fclose (f);}"]],
	  fn l => (Out.output (out, l); Out.newline out))

      fun compileAndRunC (cFile: File.t): string list =
	 (*Out.output (Out.standard, File.contents cFile)*)
	 File.withTemp
	 (fn _ => (),
	  fn exe =>
	  (docc {input = cFile, output = exe}
	   ; (File.withTemp
	      (fn _ => (),
	       fn tmp =>
	       (System.system (exe, [tmp])
		; List.map (File.lines tmp, fn s =>
			    String.dropSuffix (s, 1)))))))

      val valueLines =
	File.withTemp'
	({prefix = "/tmp/file", suffix = ".c"}, writeC, compileAndRunC)

      (* Parse the output. *)
      val values =
	 List.map2 (constants,
		    valueLines,
		    fn ((_, t), s) =>
		    case t of
		       Bool => Const.Bool (valOf (Bool.fromString s))
		     | Int => Const.Int (valOf (Int.fromString s))
		     | String => Const.String (unescape s)
		     | Real => Const.Real s
		     | Word => Const.Word (valOf (Word.fromString s)))
      val _ = Assert.assert ("buildLookupConstant", fn () =>
			     length constants = length values)
      val lookupConstant =
	 String.memoizeList (fn s => Error.bug (concat ["strange constant: ", s]),
			    List.fold2 (constants, values, [],
				       fn ((s, _), v, ac) => (s, v) :: ac))
   in lookupConstant      
   end

end
