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

fun checkPrimApp {args, prim, result, targs}: bool =
   let
      datatype z = datatype Prim.Name.t
      fun done (args', result') =
	 Vector.equals (args, Vector.fromList args', equals)
	 andalso equals (result, result')
      fun targ i = Vector.sub (targs, i)
      fun oneTarg f =
	 1 = Vector.length targs
	 andalso done (f (targ 0))
      local
	 fun make f s = let val t = f s in done ([t], t) end
      in
	 val intUnary = make int
	 val realUnary = make real
	 val wordUnary = make word
      end
      local
	 fun make f s = let val t = f s in done ([t, t], t) end
      in
	 val intBinary = make int
	 val realBinary = make real
	 val wordBinary = make word
      end
      local
	 fun make f s = let val t = f s in done ([t, t], bool) end
      in
	 val intCompare = make int
	 val realCompare = make real
	 val wordCompare = make word
      end
      fun intInfBinary () = done ([intInf, intInf, defaultWord], intInf)
      fun intInfShift () = done ([intInf, defaultWord, defaultWord], intInf)
      fun intInfUnary () = done ([intInf, defaultWord], intInf)
      fun real3 s = done ([real s, real s, real s], real s)
      val pointer = defaultWord
      val word8Array = array word8
      val wordVector = vector defaultWord
      fun wordShift s = done ([word s, defaultWord], word s)
   in
      case Prim.name prim of
	 Array_array => oneTarg (fn targ => ([defaultInt], array targ))
       | Array_array0Const => oneTarg (fn targ => ([], array targ))
       | Array_length => oneTarg (fn t => ([array t], defaultInt))
       | Array_sub => oneTarg (fn t => ([array t, defaultInt], t))
       | Array_toVector => oneTarg (fn t => ([array t], vector t))
       | Array_update => oneTarg (fn t => ([array t, defaultInt, t], unit))
       | Exn_extra => oneTarg (fn t => ([exn], t))
       | Exn_name => done ([exn], string)
       | Exn_setExtendExtra =>
	    oneTarg (fn t => ([arrow (tuple (Vector.new2 (string, t)), t)],
			      unit))
       | Exn_setInitExtra => oneTarg (fn t => ([t], unit))
       | Exn_setTopLevelHandler => done ([arrow (exn, unit)], unit)
       | FFI f => done (Vector.toList (CFunction.args f), CFunction.return f)
       | FFI_Symbol {ty, ...} => done ([], ty)
       | GC_collect => done ([], unit)
       | GC_pack => done ([], unit)
       | GC_unpack => done ([], unit)
       | IntInf_add => intInfBinary ()
       | IntInf_andb => intInfBinary ()
       | IntInf_arshift => intInfShift ()
       | IntInf_compare => done ([intInf, intInf], defaultInt)
       | IntInf_equal => done ([intInf, intInf], bool)
       | IntInf_gcd => intInfBinary ()
       | IntInf_lshift => intInfShift ()
       | IntInf_mul => intInfBinary ()
       | IntInf_neg => intInfUnary ()
       | IntInf_notb => intInfUnary ()
       | IntInf_orb => intInfBinary ()
       | IntInf_quot => intInfBinary ()
       | IntInf_rem => intInfBinary ()
       | IntInf_sub => intInfBinary ()
       | IntInf_toString => done ([intInf, defaultInt, defaultWord], string)
       | IntInf_toVector => done ([intInf], vector defaultWord)
       | IntInf_toWord => done ([intInf], defaultWord)
       | IntInf_xorb => intInfBinary ()
       | Int_add s => intBinary s
       | Int_addCheck s => intBinary s
       | Int_equal s => intCompare s
       | Int_ge s => intCompare s
       | Int_gt s => intCompare s
       | Int_le s => intCompare s
       | Int_lt s => intCompare s
       | Int_mul s => intBinary s
       | Int_mulCheck s => intBinary s
       | Int_neg s => intUnary s
       | Int_negCheck s => intUnary s
       | Int_quot s => intBinary s
       | Int_rem s => intBinary s
       | Int_sub s => intBinary s
       | Int_subCheck s => intBinary s
       | Int_toInt (s, s') => done ([int s], int s')
       | Int_toReal (s, s') => done ([int s], real s')
       | Int_toWord (s, s') => done ([int s], word s')
       | MLton_bogus => oneTarg (fn t => ([], t))
       | MLton_bug => done ([string], unit)
       | MLton_eq => oneTarg (fn t => ([t, t], bool))
       | MLton_equal => oneTarg (fn t => ([t, t], bool))
       | MLton_halt => done ([defaultInt], unit)
       | MLton_handlesSignals => done ([], bool)
       | MLton_installSignalHandler => done ([], unit)
       | MLton_size => oneTarg (fn t => ([reff t], defaultInt))
       | MLton_touch => oneTarg (fn t => ([t], unit))
       | Pointer_getInt s => done ([pointer, defaultInt], int s)
       | Pointer_getPointer => oneTarg (fn t => ([pointer, defaultInt], t))
       | Pointer_getReal s => done ([pointer, defaultInt], real s)
       | Pointer_getWord s => done ([pointer, defaultInt], word s)
       | Pointer_setInt s => done ([pointer, defaultInt, int s], unit)
       | Pointer_setPointer => oneTarg (fn t => ([pointer, defaultInt, t], unit))
       | Pointer_setReal s => done ([pointer, defaultInt, real s], unit)
       | Pointer_setWord s => done ([pointer, defaultInt, word s], unit)
       | Real_Math_acos s => realUnary s
       | Real_Math_asin s => realUnary s
       | Real_Math_atan s => realUnary s
       | Real_Math_atan2 s => realBinary s
       | Real_Math_cos s => realUnary s
       | Real_Math_exp s => realUnary s
       | Real_Math_ln s => realUnary s
       | Real_Math_log10 s => realUnary s
       | Real_Math_sin s => realUnary s
       | Real_Math_sqrt s => realUnary s
       | Real_Math_tan s => realUnary s
       | Real_abs s => realUnary s
       | Real_add s => realBinary s
       | Real_div s => realBinary s
       | Real_equal s => realCompare s
       | Real_ge s => realCompare s
       | Real_gt s => realCompare s
       | Real_ldexp s => done ([real s, defaultInt], real s)
       | Real_le s => realCompare s
       | Real_lt s => realCompare s
       | Real_mul s => realBinary s
       | Real_muladd s => real3 s
       | Real_mulsub s => real3 s
       | Real_neg s => realUnary s
       | Real_qequal s => realCompare s
       | Real_round s => realUnary s
       | Real_sub s => realBinary s
       | Real_toInt (s, s') => done ([real s], int s')
       | Real_toReal (s, s') => done ([real s], real s')
       | Ref_assign => oneTarg (fn t => ([reff t, t], unit))
       | Ref_deref => oneTarg (fn t => ([reff t], t))
       | Ref_ref => oneTarg (fn t => ([t], reff t))
       | Thread_atomicBegin => done ([], unit)
       | Thread_atomicEnd => done ([], unit)
       | Thread_canHandle => done ([], defaultInt)
       | Thread_copy => done ([thread], thread)
       | Thread_copyCurrent => done ([], unit)
       | Thread_returnToC => done ([], unit)
       | Thread_switchTo => done ([thread], unit)
       | Vector_length => oneTarg (fn t => ([vector t], defaultInt))
       | Vector_sub => oneTarg (fn t => ([vector t, defaultInt], t))
       | Weak_canGet => oneTarg (fn t => ([weak t], bool))
       | Weak_get => oneTarg (fn t => ([weak t], t))
       | Weak_new => oneTarg (fn t => ([t], weak t))
       | Word8Array_subWord => done ([word8Array, defaultInt], defaultWord)
       | Word8Array_updateWord =>
	    done ([word8Array, defaultInt, defaultWord], unit)
       | Word8Vector_subWord => done ([word8Vector, defaultInt], defaultWord)
       | WordVector_toIntInf => done ([wordVector], intInf)
       | Word_add s => wordBinary s
       | Word_addCheck s => wordBinary s
       | Word_andb s => wordBinary s
       | Word_arshift s => wordShift s
       | Word_div s => wordBinary s
       | Word_equal s => wordCompare s
       | Word_ge s => wordCompare s
       | Word_gt s => wordCompare s
       | Word_le s => wordCompare s
       | Word_lshift s => wordShift s
       | Word_lt s => wordCompare s
       | Word_mod s => wordBinary s
       | Word_mul s => wordBinary s
       | Word_mulCheck s => wordBinary s
       | Word_neg s => wordUnary s
       | Word_notb s => wordUnary s
       | Word_orb s => wordBinary s
       | Word_rol s => wordShift s
       | Word_ror s => wordShift s
       | Word_rshift s => wordShift s
       | Word_sub s => wordBinary s
       | Word_toInt (s, s') => done ([word s], int s')
       | Word_toIntInf => done ([defaultWord], intInf)
       | Word_toIntX (s, s') => done ([word s], int s')
       | Word_toWord (s, s') => done ([word s], word s')
       | Word_toWordX (s, s') => done ([word s], word s')
       | Word_xorb s => wordBinary s
       | World_save => done ([defaultWord], unit)
       | _ => Error.bug (concat ["Type.checkPrimApp got strange prim: ",
				 Prim.toString prim])
   end

end
