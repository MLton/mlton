(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor HashType (S: HASH_TYPE_STRUCTS): HASH_TYPE = 
struct

open S

structure Type =
   struct
      datatype t =
	 T of {
	       hash: Word.t,
	       plist: PropertyList.t,
	       tree: tree
	       }
      and tree =
	 Var of Tyvar.t
	| Con of Tycon.t * t vector

      local
	 fun make f (T r) = f r
      in
	 val hash = make #hash
	 val plist = make #plist
	 val tree = make #tree
      end

      local
	 open Layout
      in
	 val rec layoutTree =
	    fn Var a => Tyvar.layout a
	     | Con (c, ts) =>
		  seq [Tycon.layout c,
		       Vector.layout (layoutTree o tree) ts]
      end

      structure Dest =
	 struct
	    datatype dest = datatype tree
	    val dest = tree
	 end
      open Dest

      fun deConOpt t =
	 case dest t of
	    Con x => SOME x
	  | _ => NONE

      fun makeHom {con, var} =
	 let
	    val {get, destroy, ...} =
	       Property.destGet
	       (plist,
		Property.initRec
		(fn (t, get) =>
		 case dest t of
		    Var a => var (t, a)
		  | Con (c, ts) => con (t, c, Vector.map (ts, get))))
	 in {hom = get, destroy = destroy}
	 end

      fun hom {ty, var, con} =
	 let
	    val {hom, destroy} = makeHom {var = var o #2,
					  con = fn (_, c, xs) => con (c, xs)}
	    val res = hom ty
	    val _ = destroy ()
	 in res
	 end

      fun makeMonoHom {con} =
	 makeHom {var = fn _ => Error.bug "makeMonoHom saw type variable",
		  con = con}

      fun equals (t, t'): bool = PropertyList.equals (plist t, plist t')
		      
      fun layout (ty: t): Layout.t =
	 #1 (hom {con = Tycon.layoutApp,
		  ty = ty,
		  var = fn a => (Tyvar.layout a, {isChar = false,
						  needsParen = false})})

      val toString = Layout.toString o layout
	 
      local
	 val same: tree * tree -> bool =
	    fn (Var a, Var a') => Tyvar.equals (a, a')
	     | (Con (c, ts), Con (c', ts')) =>
		  Tycon.equals (c, c')
		  andalso Vector.equals (ts, ts', equals)
	     | _ => false
	 val same =
	    Trace.trace2 ("Type.same", layoutTree, layoutTree, Bool.layout)
	    same
	 val table: t HashSet.t = HashSet.new {hash = hash}
      in
	 fun lookup (hash, tr) =
	    HashSet.lookupOrInsert (table, hash,
				    fn t => same (tr, tree t),
				    fn () => T {hash = hash,
						plist = PropertyList.new (),
						tree = tr})

	 fun stats () =
	    let open Layout
	    in align [seq [str "num distinct types = ",
			   Int.layout (HashSet.size table)],
		      Control.sizeMessage ("hash table", table)]
	    end
      end

      fun var a = lookup (Tyvar.hash a, Var a)

      local
	 val generator: Word.t = 0wx5555
      in
	 fun con (c, ts) =
	    lookup (Vector.fold (ts, Tycon.hash c, fn (t, w) =>
				 Word.xorb (w * generator, hash t)),
		    Con (c, ts))
	 val con = Trace.trace2 ("Type.con",
				 Tycon.layout,
				 Vector.layout layout,
				 layout) con
      end
   end
structure Ops = TypeOps (structure Tycon = Tycon
			 open Type)
open Type Ops

val string = word8Vector
   
fun ofConst c =
   let
      datatype z = datatype Const.t
   in
      case c of
	 Int i => int (IntX.size i)
       | IntInf _ => intInf
       | Real r => real (RealX.size r)
       | Word w => word (WordX.size w)
       | Word8Vector _ => word8Vector
   end

fun isUnit t =
   case dest t of
      Con (c, ts) => 0 = Vector.length ts andalso Tycon.equals (c, Tycon.tuple)
    | _ => false

fun substitute (ty, v) =
   if Vector.isEmpty v
      then ty (* This optimization is important so that monotypes
	       * are not substituted inside of.
	       *)
   else 
      hom {ty = ty,
	   var = fn a => (case Vector.peek (v, fn (a', _) =>
					    Tyvar.equals (a, a')) of
			     NONE => var a
			   | SOME (_, ty) => ty),
	   con = con}

(* val substitute =
 *    Trace.trace2 ("substitute", layout,
 * 		List.layout (Layout.tuple2 (Tyvar.layout, Type.layout)),
 * 		layout) substitute		
 *)

(* fun equalss (ts: t list): t option =
 *    case ts of
 *       t :: ts =>
 * 	 let fun loop [] = SOME t
 * 	       | loop (t' :: ts) = if equals (t, t') then loop ts else NONE
 * 	 in loop ts
 * 	 end
 *     | [] => Error.bug "equals"
 *)

local
   val out = Out.error
   val print = Out.outputc out
   exception TypeError
in
   fun error (msg, lay) =
      (print (concat ["Type error: ", msg, "\n"])
       ; Layout.output (lay, out)
       ; print "\n"
       ; raise TypeError)
end

fun tycon t =
   case dest t of
      Con (c, _) => c
    | _ => Error.bug "Type.tycon saw type variable"

fun containsTycon (ty, tycon) =
   hom {ty = ty,
	var = fn _ => false,
	con = fn (tycon', bs) => (Tycon.equals (tycon, tycon')
				  orelse Vector.exists (bs, fn b => b))}

end
