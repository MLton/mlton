(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
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

structure P = PointerTycon
   
fun fromRepType (t: RepType.t): t =
   let
      fun bug () = Error.bug (concat ["Type.fromRepType: ", RepType.toString t])
      datatype z = datatype RepType.dest
   in
      case RepType.dest t of
	 Int s => int s
       | Real s => real s
       | Pointer p =>
	    (case List.peek ([(P.thread, thread),
			      (P.word8Vector, word8Vector)],
			     fn (p', _) => P.equals (p, p')) of
		NONE => bug ()
	      | SOME (_, t) => t)
       | Seq ts => if 0 = Vector.length ts then unit else bug ()
       | Sum _ => if RepType.isBool t then bool else bug ()
       | Word s => word (WordSize.fromBits s)
       | _ => bug ()
   end

val fromRepType =
   Trace.trace ("Type.fromRepType", RepType.layout, layout) fromRepType

local
   val {get, set, ...} =
      Property.getSetOnce (Tycon.plist, Property.initConst NONE)
   val () =
      List.foreach ([Tycon.array, Tycon.reff, Tycon.vector], fn t =>
		    set (t, SOME (RepType.cPointer ())))
   fun doit (ts, f) = Vector.foreach (ts, fn (c, s) => set (c, SOME (f s)))
   val () = doit (Tycon.ints, RepType.int)
   val () = doit (Tycon.reals, RepType.real)
   val () = set (Tycon.thread, SOME RepType.thread)
   val () = doit (Tycon.words, RepType.word o WordSize.bits)
in
   fun toRepType (t: t): RepType.t =
      let
	 fun bug () = Error.bug (concat ["Type.toRepType: ", toString t])
      in
	 case dest t of
	    Con (c, _) =>
	       (case get c of
		   NONE => bug ()
		 | SOME t => t)
	  | Var _ => bug ()
      end
end

fun checkPrimApp {args, prim, result}: bool =
   let
      fun check () =
	 case Prim.typeCheck (prim, Vector.map (args, toRepType)) of
	    NONE => false
	  | SOME t => equals (result, fromRepType t)
      datatype z = datatype Prim.Name.t
   in
      case Prim.name prim of
	 Array_array => true
       | Array_array0Const => true
       | Array_length => true
       | Array_sub => true
       | Array_toVector => true
       | Array_update => true
       | Exn_extra => true
       | Exn_name => true
       | Exn_setExtendExtra => true
       | Exn_setInitExtra => true
       | Exn_setTopLevelHandler => true
       | GC_collect => true
       | GC_pack => true
       | GC_unpack => true
       | IntInf_add => true
       | IntInf_andb => true
       | IntInf_arshift => true
       | IntInf_compare => true
       | IntInf_equal => true
       | IntInf_gcd => true
       | IntInf_lshift => true
       | IntInf_mul => true
       | IntInf_neg => true
       | IntInf_notb => true
       | IntInf_orb => true
       | IntInf_quot => true
       | IntInf_rem => true
       | IntInf_sub => true
       | IntInf_toString => true
       | IntInf_toVector => true
       | IntInf_toWord => true
       | IntInf_xorb => true
       | MLton_bogus => true
       | MLton_bug => true
       | MLton_eq => true
       | MLton_equal => true
       | MLton_halt => true
       | MLton_handlesSignals => true
       | MLton_installSignalHandler => true
       | MLton_size => true
       | MLton_touch => true
       | Pointer_getInt _ => true
       | Pointer_getPointer => true
       | Pointer_getReal _ => true
       | Pointer_getWord _ => true
       | Pointer_setInt _ => true
       | Pointer_setPointer => true
       | Pointer_setReal _ => true
       | Pointer_setWord _ => true
       | Ref_assign => true
       | Ref_deref => true
       | Ref_ref => true
       | Thread_atomicBegin => true
       | Thread_atomicEnd => true
       | Thread_canHandle => true
       | Thread_copy => true
       | Thread_copyCurrent => true
       | Thread_returnToC => true
       | Thread_switchTo => true
       | Vector_length => true
       | Vector_sub => true
       | Weak_canGet => true
       | Weak_get => true
       | Weak_new => true
       | Word_toIntInf => true
       | WordVector_toIntInf => true
       | Word8Array_subWord => true
       | Word8Array_updateWord => true
       | Word8Vector_subWord => true
       | World_save => true
       | _ => check ()
   end

end
