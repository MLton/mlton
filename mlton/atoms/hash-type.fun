(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
         makeHom {var = fn _ => Error.bug "HashType.Type.makeMonoHom: type variable",
                  con = con}

      fun equals (t, t'): bool = PropertyList.equals (plist t, plist t')

      fun layout (ty: t): Layout.t =
         #1 (hom {con = Tycon.layoutApp,
                  ty = ty,
                  var = fn a => (Tyvar.layout a,
                                 ({isChar = false},
                                  Tycon.BindingStrength.unit))})

      local
         val same: tree * tree -> bool =
            fn (Var a, Var a') => Tyvar.equals (a, a')
             | (Con (c, ts), Con (c', ts')) =>
                  Tycon.equals (c, c')
                  andalso Vector.equals (ts, ts', equals)
             | _ => false
         val same =
            Trace.trace2 ("HashType.Type.same", layoutTree, layoutTree, Bool.layout)
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
            in align [seq [str "num types in hash table = ",
                           Int.layout (HashSet.size table)],
                      Control.sizeMessage ("types hash table", table)]
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
         val con = Trace.trace2 ("HashType.Type.con",
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
         IntInf _ => intInf
       | Null => cpointer
       | Real r => real (RealX.size r)
       | Word w => word (WordX.size w)
       | WordVector v => vector (word (WordXVector.elementSize v))
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

val substitute =
   Trace.trace2 
   ("HashType.substitute", 
    layout, 
    Vector.layout (Layout.tuple2 (Tyvar.layout, Type.layout)), 
    layout) 
   substitute

(* fun equalss (ts: t list): t option =
 *    case ts of
 *       t :: ts =>
 *       let fun loop [] = SOME t
 *             | loop (t' :: ts) = if equals (t, t') then loop ts else NONE
 *       in loop ts
 *       end
 *     | [] => Error.bug "HashType.equals"
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
    | _ => Error.bug "HashType.tycon: type variable"

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
         val realUnary = make real
         val wordUnary = make word
      end
      local
         fun make f s = let val t = f s in done ([t, t], t) end
      in
         val realBinary = make real
         val wordBinary = make word
      end
      local
         fun make f s = let val t = f s in done ([t, t], bool) end
      in
         val realCompare = make real
         val wordCompare = make word
      end
      val cint = word (WordSize.cint ())
      val compareRes = word WordSize.compareRes
      val csize = word (WordSize.csize ())
      val cptrdiff = word (WordSize.cptrdiff ())
      val seqIndex = word (WordSize.seqIndex ())
      val shiftArg = word WordSize.shiftArg
      val bigIntInfWord = word (WordSize.bigIntInfWord ())
      val smallIntInfWord = word (WordSize.smallIntInfWord ())

      fun intInfBinary () = done ([intInf, intInf, csize], intInf)
      fun intInfShift () = done ([intInf, shiftArg, csize], intInf)
      fun intInfUnary () = done ([intInf, csize], intInf)
      fun realTernary s = done ([real s, real s, real s], real s)
      val word8Array = array word8
      fun wordShift s = done ([word s, shiftArg], word s)
   in
      case Prim.name prim of
         Array_array => oneTarg (fn targ => ([seqIndex], array targ))
       | Array_array0Const => oneTarg (fn targ => ([], array targ))
       | Array_length => oneTarg (fn t => ([array t], seqIndex))
       | Array_sub => oneTarg (fn t => ([array t, seqIndex], t))
       | Array_toVector => oneTarg (fn t => ([array t], vector t))
       | Array_update => oneTarg (fn t => ([array t, seqIndex, t], unit))
       | CPointer_add => done ([cpointer, csize], cpointer) 
       | CPointer_diff => done ([cpointer, cpointer], csize)
       | CPointer_equal => done ([cpointer, cpointer], bool)
       | CPointer_fromWord => done ([csize], cpointer)
       | CPointer_getCPointer => done ([cpointer, cptrdiff], cpointer)
       | CPointer_getObjptr => oneTarg (fn t => ([cpointer, cptrdiff], t))
       | CPointer_getReal s => done ([cpointer, cptrdiff], real s)
       | CPointer_getWord s => done ([cpointer, cptrdiff], word s)
       | CPointer_lt => done ([cpointer, cpointer], bool)
       | CPointer_setCPointer => done ([cpointer, cptrdiff, cpointer], unit)
       | CPointer_setObjptr => oneTarg (fn t => ([cpointer, cptrdiff, t], unit))
       | CPointer_setReal s => done ([cpointer, cptrdiff, real s], unit)
       | CPointer_setWord s => done ([cpointer, cptrdiff, word s], unit)
       | CPointer_sub => done ([cpointer, csize], cpointer)
       | CPointer_toWord => done ([cpointer], csize)
       | Exn_extra => oneTarg (fn t => ([exn], t))
       | Exn_name => done ([exn], string)
       | Exn_setExtendExtra => oneTarg (fn t => ([arrow (t, t)], unit))
       | Exn_setInitExtra => oneTarg (fn t => ([t], unit))
       | FFI f => done (Vector.toList (CFunction.args f), CFunction.return f)
       | FFI_Symbol _ => done ([], cpointer)
       | GC_collect => done ([], unit)
       | IntInf_add => intInfBinary ()
       | IntInf_andb => intInfBinary ()
       | IntInf_arshift => intInfShift ()
       | IntInf_compare => done ([intInf, intInf], compareRes)
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
       | IntInf_toString => done ([intInf, word32, csize], string)
       | IntInf_toVector => done ([intInf], vector bigIntInfWord)
       | IntInf_toWord => done ([intInf], smallIntInfWord)
       | IntInf_xorb => intInfBinary ()
       | MLton_bogus => oneTarg (fn t => ([], t))
       | MLton_bug => done ([string], unit)
       | MLton_eq => oneTarg (fn t => ([t, t], bool))
       | MLton_equal => oneTarg (fn t => ([t, t], bool))
       | MLton_halt => done ([cint], unit)
       | MLton_hash => oneTarg (fn t => ([seqIndex, t], word32))
       | MLton_handlesSignals => done ([], bool)
       | MLton_installSignalHandler => done ([], unit)
       | MLton_share => oneTarg (fn t => ([t], unit))
       | MLton_size => oneTarg (fn t => ([t], csize))
       | MLton_touch => oneTarg (fn t => ([t], unit))
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
       | Real_castToWord (s, s') => done ([real s], word s')
       | Real_div s => realBinary s
       | Real_equal s => realCompare s
       | Real_ldexp s => done ([real s, cint], real s)
       | Real_le s => realCompare s
       | Real_lt s => realCompare s
       | Real_mul s => realBinary s
       | Real_muladd s => realTernary s
       | Real_mulsub s => realTernary s
       | Real_neg s => realUnary s
       | Real_qequal s => realCompare s
       | Real_rndToReal (s, s') => done ([real s], real s')
       | Real_rndToWord (s, s', _) => done ([real s], word s')
       | Real_round s => realUnary s
       | Real_sub s => realBinary s
       | Ref_assign => oneTarg (fn t => ([reff t, t], unit))
       | Ref_deref => oneTarg (fn t => ([reff t], t))
       | Ref_ref => oneTarg (fn t => ([t], reff t))
       | Thread_atomicBegin => done ([], unit)
       | Thread_atomicEnd => done ([], unit)
       | Thread_atomicState => done ([], word32)
       | Thread_copy => done ([thread], thread)
       | Thread_copyCurrent => done ([], unit)
       | Thread_returnToC => done ([], unit)
       | Thread_switchTo => done ([thread], unit)
       | TopLevel_getHandler => done ([], arrow (exn, unit))
       | TopLevel_getSuffix => done ([], arrow (unit, unit))
       | TopLevel_setHandler => done ([arrow (exn, unit)], unit)
       | TopLevel_setSuffix => done ([arrow (unit, unit)], unit)
       | Vector_length => oneTarg (fn t => ([vector t], seqIndex))
       | Vector_sub => oneTarg (fn t => ([vector t, seqIndex], t))
       | Weak_canGet => oneTarg (fn t => ([weak t], bool))
       | Weak_get => oneTarg (fn t => ([weak t], t))
       | Weak_new => oneTarg (fn t => ([t], weak t))
       | Word8Array_subWord s => done ([word8Array, seqIndex], word s)
       | Word8Array_updateWord s => done ([word8Array, seqIndex, word s], unit)
       | Word8Vector_subWord s => done ([word8Vector, seqIndex], word s)
       | WordVector_toIntInf => done ([vector bigIntInfWord], intInf)
       | Word_add s => wordBinary s
       | Word_addCheck (s, _) => wordBinary s
       | Word_andb s => wordBinary s
       | Word_castToReal (s, s') => done ([word s], real s')
       | Word_equal s => wordCompare s
       | Word_extdToWord (s, s', _) => done ([word s], word s')
       | Word_lshift s => wordShift s
       | Word_lt (s, _) => wordCompare s
       | Word_mul (s, _) => wordBinary s
       | Word_mulCheck (s, _) => wordBinary s
       | Word_neg s => wordUnary s
       | Word_negCheck s => wordUnary s
       | Word_notb s => wordUnary s
       | Word_orb s => wordBinary s
       | Word_quot (s, _) => wordBinary s
       | Word_rem (s, _) => wordBinary s
       | Word_rndToReal (s, s', _) => done ([word s], real s')
       | Word_rol s => wordShift s
       | Word_ror s => wordShift s
       | Word_rshift (s, _) => wordShift s
       | Word_sub s => wordBinary s
       | Word_subCheck (s, _) => wordBinary s
       | Word_toIntInf => done ([smallIntInfWord], intInf)
       | Word_xorb s => wordBinary s
       | World_save => done ([string], unit)
       | _ => Error.bug (concat ["HashType.checkPrimApp: strange prim: ",
                                 Prim.toString prim])
   end

end
