(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AbstractValue (S: ABSTRACT_VALUE_STRUCTS): ABSTRACT_VALUE = 
struct

open S
open Sxml

structure Dset = DisjointSet

structure Lambda =
   struct
      datatype t = Lambda of {lambda: Sxml.Lambda.t,
                              hash: Word.t}

      val newHash = Random.word

      fun new lambda = Lambda {lambda = lambda,
                               hash = newHash ()}

      fun hash (Lambda {hash, ...}) = hash

      fun dest (Lambda {lambda, ...}) = lambda

      fun equals (Lambda r, Lambda r') =
         #hash r = #hash r'
         andalso Sxml.Lambda.equals (#lambda r, #lambda r')

      fun layout (Lambda {lambda, ...}) =
         let open Layout
         in seq [str "lambda ", Sxml.Var.layout (Sxml.Lambda.arg lambda)]
         end
   end

structure Lambdas = UniqueSet (structure Element = Lambda
                               val cacheSize: int = 5
                               val bits: int = 13)

structure LambdaNode:
   sig
      type t

      val addHandler: t * (Lambda.t -> unit) -> unit
      val coerce: {from: t, to: t} -> unit
      val lambda: Sxml.Lambda.t -> t
      val layout: t -> Layout.t
      val new: unit -> t
      val toSet: t -> Lambdas.t
      val unify: t * t -> unit
   end =
   struct
      datatype t = LambdaNode of {me: Lambdas.t ref,
                                  handlers: (Lambda.t -> unit) list ref,
                                  coercedTo: t list ref} Dset.t

      fun toSet (LambdaNode d) = !(#me (Dset.! d))

      val layout = Lambdas.layout o toSet

      fun newSet s = LambdaNode (Dset.singleton {me = ref s,
                                                 handlers = ref [],
                                                 coercedTo = ref []})

      fun new () = newSet Lambdas.empty

      fun lambda l = newSet (Lambdas.singleton (Lambda.new l))

      fun handles (h: Lambda.t -> unit, s: Lambdas.t): unit =
         Lambdas.foreach (s, fn l => h l)

      fun handless (hs: (Lambda.t -> unit) list, s: Lambdas.t): unit =
         List.foreach (hs, fn h => handles (h, s))

      fun addHandler (LambdaNode d, h: Lambda.t -> unit) =
         let val {me, handlers, ...} = Dset.! d
         in List.push (handlers, h)
            ; handles (h, !me)
         end

      fun send (LambdaNode d, s): unit =
         let val {me, coercedTo, handlers, ...} = Dset.! d
            val diff = Lambdas.- (s, !me)
         in if Lambdas.isEmpty diff
               then ()
            else (me := Lambdas.+ (diff, !me)
                  ; List.foreach (!coercedTo, fn to => send (to, diff))
                  ; handless (!handlers, diff))
         end

      val send =
         Trace.trace2 
         ("AbstractValue.LambdaNode.send", 
          layout, Lambdas.layout, Unit.layout)
         send

      fun equals (LambdaNode d, LambdaNode d') = Dset.equals (d, d')

      fun coerce {from = from as LambdaNode d, to: t}: unit =
         if equals (from, to)
            then ()
         else let
                 val {me, coercedTo, ...} = Dset.! d
              in
                 if List.exists (!coercedTo, fn ls => equals (ls, to))
                    then ()
                 else (List.push (coercedTo, to)
                       ; send (to, !me))
              end

      fun update (c, h, diff) =
         if Lambdas.isEmpty diff
            then ()
         else (List.foreach (c, fn to => send (to, diff))
               ; handless (h, diff))

      fun unify (LambdaNode d, LambdaNode d'): unit =
         if Dset.equals (d, d')
            then ()
         else
            let
               val {me = ref m, coercedTo = ref c, handlers = ref h, ...} =
                  Dset.! d
               val {me = ref m', coercedTo = ref c', handlers = ref h', ...} =
                  Dset.! d'
               val diff = Lambdas.- (m, m')
               val diff' = Lambdas.- (m', m)
            in Dset.union (d, d')
               ; (Dset.:=
                  (d, {me = ref (if Lambdas.isEmpty diff
                                   then m'
                                else Lambdas.+ (m', diff)),
                       coercedTo = ref (List.fold
                                        (c', c, fn (n', ac) =>
                                         if List.exists (c, fn n =>
                                                         equals (n, n'))
                                            then ac
                                         else n' :: ac)),
                       handlers = ref (List.appendRev (h, h'))}))
               ; update (c, h, diff')
               ; update (c', h', diff)
            end

(*
      val unify =
         Trace.trace2 
         ("AbstractValue.LambdaNode.unify", layout, layout, Unit.layout) 
         unify
*)       
   end

structure UnaryTycon =
   struct
      datatype t = Array | Ref | Vector | Weak

      val toString =
         fn Array => "Array"
          | Ref => "Ref"
          | Vector => "Vector"
          | Weak => "Weak"

      val equals: t * t -> bool = op =

      val layout = Layout.str o toString
   end

datatype tree =
   Lambdas of LambdaNode.t
 | Tuple of t vector
 | Type of Type.t
 | Unify of UnaryTycon.t * t

withtype t = {tree: tree,
              ty: Type.t,
              ssaType: Ssa.Type.t option ref} Dset.t

fun new (tree: tree, ty: Type.t): t =
   Dset.singleton {ssaType = ref NONE,
                   tree = tree,
                   ty = ty}

local
   fun make sel : t -> 'a = sel o Dset.!
in
   val ssaType = make #ssaType
   val tree = make #tree
   val ty = make #ty
end

fun layout v =
   let open Layout
   in case tree v of
      Type t => seq [str "Type ", Type.layout t]
    | Unify (t, v) => paren (seq [UnaryTycon.layout t, str " ", layout v])
    | Tuple vs => Vector.layout layout vs
    | Lambdas l => LambdaNode.layout l
   end

fun isEmpty v =
   case tree v of
      Lambdas n => Lambdas.isEmpty (LambdaNode.toSet n)
    | Tuple vs => Vector.exists (vs, isEmpty)
    | Unify (UnaryTycon.Ref, v) => isEmpty v
    | _ => false

(* used in closure converter *)
fun equals (v, v') =
   Dset.equals (v, v')
   orelse
   (case (tree v,      tree v') of
       (Type t,        Type t')   =>
          if Type.equals (t, t')
             then true
          else Error.bug "AbstractValue.equals: different types"
     | (Unify (t, v), Unify (t', v'))     =>
          UnaryTycon.equals (t, t') andalso equals (v, v')
     | (Tuple vs,  Tuple vs')  => Vector.forall2 (vs, vs', equals)
     | (Lambdas n, Lambdas n') => Lambdas.equals (LambdaNode.toSet n,
                                                 LambdaNode.toSet n')
     | _ => Error.bug "AbstractValue.equals: different values")

fun addHandler (v, h) =
   case tree v of
      Lambdas n => LambdaNode.addHandler (n, h)
    | _ => Error.bug "AbstractValue.addHandler: non-lambda"

local
   val {hom, destroy} =
      Type.makeMonoHom
      {con = fn (t, tycon, vs) =>
       let val new = fn tree => new (tree, t)
       in if Tycon.equals (tycon, Tycon.arrow)
             then {isFirstOrder = false,
                   make = fn () => new (Lambdas (LambdaNode.new ()))}
          else
             if Vector.forall (vs, #isFirstOrder)
                then {isFirstOrder = true,
                      make = let val v = new (Type t)
                             in fn () => v
                             end}
             else
                {isFirstOrder = false,
                 make = let
                           fun mutable mt =
                              let val make = #make (Vector.first vs)
                              in fn () => new (Unify (mt, make ()))
                              end
                        in if Tycon.equals (tycon, Tycon.reff)
                              then mutable UnaryTycon.Ref
                           else if Tycon.equals (tycon, Tycon.array)
                                   then mutable UnaryTycon.Array
                           else if Tycon.equals (tycon, Tycon.vector)
                                   then mutable UnaryTycon.Vector
                           else if Tycon.equals (tycon, Tycon.weak)
                                   then mutable UnaryTycon.Weak
                           else if Tycon.equals (tycon, Tycon.tuple)
                                   then (fn () =>
                                         new (Tuple
                                              (Vector.map (vs, fn {make, ...} =>
                                                           make ()))))
                           else Error.bug "AbstractValue.fromType: non-arrow"
                        end}
       end}
in
   val destroy = destroy
   val typeIsFirstOrder = #isFirstOrder o hom
   fun fromType t = #make (hom t) ()
end

val fromType = Trace.trace ("AbstractValue.fromType", Type.layout, layout) fromType

fun tuple (vs: t vector): t = new (Tuple vs,
                                   Type.tuple (Vector.map (vs, ty)))

fun select (v, i) =
   case tree v of
      Type t => fromType (Vector.sub (Type.deTuple t, i))
    | Tuple vs => Vector.sub (vs, i)
    | _ => Error.bug "AbstractValue.select: expected tuple"

fun deRef v =
   case tree v of
      Type t => fromType (Type.deRef t)
    | Unify (_, v) => v
    | _ => Error.bug "AbstractValue.deRef"

val deRef = Trace.trace ("AbstractValue.deRef", layout, layout) deRef

fun deWeak v =
   case tree v of
      Type t => fromType (Type.deWeak t)
    | Unify (_, v) => v
    | _ => Error.bug "AbstractValue.deWeak"

fun deArray v =
   case tree v of
      Type t => fromType (Type.deArray t)
    | Unify (_, v) => v
    | _ => Error.bug "AbstractValue.deArray"

fun deVector v =
   case tree v of
      Type t => fromType (Type.deVector t)
    | Unify (_, v) => v
    | _ => Error.bug "AbstractValue.deVector"

fun lambda (l: Sxml.Lambda.t, t: Type.t): t =
   new (Lambdas (LambdaNode.lambda l), t)       

fun unify (v, v') =
   if Dset.equals (v, v')
      then ()
   else let val t = tree v
            val t' = tree v'
        in Dset.union (v, v')
           ; (case (t, t') of
                 (Type t, Type t') => if Type.equals (t, t')
                                         then ()
                                      else Error.bug "AbstractValue.unify: different types"
               | (Unify (_, v), Unify (_, v')) => unify (v, v')
               | (Tuple vs, Tuple vs') => Vector.foreach2 (vs, vs', unify)
               | (Lambdas l, Lambdas l') => LambdaNode.unify (l, l')
               | _ => Error.bug "AbstractValue.unify: different values")
        end

val unify = Trace.trace2 ("AbstractValue.unify", layout, layout, Unit.layout) unify

fun coerce {from: t, to: t}: unit =
   if Dset.equals (from, to)
      then ()
   else (case (tree from, tree to) of
            (Type t,    Type t')    => if Type.equals (t, t')
                                          then ()
                                       else Error.bug "coerce"
          | (Unify _, Unify _) =>
               (* Can't do a coercion for vectors, since that would imply
                * walking over the entire vector and coercing each element
                *)
               unify (from, to)
          | (Tuple vs,  Tuple vs')  =>
               Vector.foreach2 (vs, vs', fn (v, v') =>
                                coerce {from = v, to = v'})
          | (Lambdas l, Lambdas l') => LambdaNode.coerce {from = l, to = l'}
          | _ => Error.bug "AbstractValue.coerce: different values")

val coerce = Trace.trace ("AbstractValue.coerce",
                          fn {from, to} =>
                          let open Layout
                          in record [("from", layout from),
                                     ("to" , layout to)]
                          end, Unit.layout) coerce

structure Dest =
   struct
      datatype dest =
         Array of t
       | Lambdas of Lambdas.t
       | Ref of t
       | Tuple of t vector
       | Type of Type.t
       | Vector of t
       | Weak of t
   end

fun dest v =
   case tree v of
      Type t => Dest.Type t
    | Unify (mt, v) => (case mt of
                           UnaryTycon.Array => Dest.Array v
                         | UnaryTycon.Ref => Dest.Ref v
                         | UnaryTycon.Vector => Dest.Vector v
                         | UnaryTycon.Weak => Dest.Weak v)
    | Tuple vs => Dest.Tuple vs
    | Lambdas l => Dest.Lambdas (LambdaNode.toSet l)

open Dest

(*---------------------------------------------------*)
(*                     primApply                     *)
(*---------------------------------------------------*)

val {get = serialValue: Type.t -> t, ...} =
   Property.get (Type.plist, Property.initFun fromType)

fun primApply {prim: Type.t Prim.t, args: t vector, resultTy: Type.t}: t =
   let 
      fun result () = fromType resultTy
      fun typeError () =
         (Control.message
          (Control.Silent, fn () =>
           let open Layout
           in align [seq [str "prim: ", Prim.layout prim],
                     seq [str "args: ", Vector.layout layout args]]
           end)
          ; Error.bug "AbstractValue.primApply: type error")
      fun arg i = Vector.sub (args, i)
      val n = Vector.length args
      fun oneArg () =
         if n = 1
            then arg 0
         else Error.bug "AbstractValue.primApply.oneArg"
      fun twoArgs () =
         if n = 2
            then (arg 0, arg 1)
         else Error.bug "AbstractValue.primApply.twoArgs"
      fun threeArgs () =
         if n = 3
            then (arg 0, arg 1, arg 2)
         else Error.bug "AbstractValue.primApply.threeArgs"
      fun fiveArgs () =
         if n = 5
            then (arg 0, arg 1, arg 2, arg 3, arg 4)
         else Error.bug "AbstractValue.primApply.fiveArgs"
      datatype z = datatype Prim.Name.t
   in
      case Prim.name prim of
         Array_copyArray =>
            let val (da, _, sa, _, _) = fiveArgs ()
            in (case (dest da, dest sa) of
                   (Array dx, Array sx) => unify (dx, sx)
                 | (Type _, Type _) => ()
                 | _ => typeError ()
                ; result ())
            end
       | Array_copyVector =>
            let val (da, _, sa, _, _) = fiveArgs ()
            in (case (dest da, dest sa) of
                   (Array dx, Vector sx) => unify (dx, sx)
                 | (Type _, Type _) => ()
                 | _ => typeError ()
                ; result ())
            end
       | Array_toArray =>
            let val r = result ()
            in (case (dest (oneArg ()), dest r) of
                   (Type _, Type _) => ()
                 | (Array x, Array y) =>
                      (* Can't do a coercion here because that would imply
                       * walking over each element of the array and coercing it.
                       *)
                      unify (x, y)
                 | _ => typeError ())
               ; r
            end
       | Array_toVector =>
            let val r = result ()
            in (case (dest (oneArg ()), dest r) of
                   (Type _, Type _) => ()
                 | (Array x, Vector y) =>
                      (* Can't do a coercion here because that would imply
                       * walking over each element of the array and coercing it.
                       *)
                      unify (x, y)
                 | _ => typeError ())
               ; r
            end
       | Array_sub =>
            (case dest (#1 (twoArgs ())) of
                Array x => x
              | Type _ => result ()
              | _ => typeError ())
       | Array_update =>
            let val (a, _, x) = threeArgs ()
            in (case dest a of
                   Array x' => coerce {from = x, to = x'} (* unify (x, x') *)
                 | Type _ => ()
                 | _ => typeError ())
               ; result ()
            end
       | MLton_deserialize => serialValue resultTy
       | MLton_serialize =>
            let val arg = oneArg ()
            in coerce {from = arg, to = serialValue (ty arg)}
               ; result ()
            end
       | Ref_assign =>
            let val (r, x) = twoArgs ()
            in (case dest r of
                   Ref x' => coerce {from = x, to = x'} (* unify (x, x') *)
                 | Type _ => ()
                 | _ => typeError ())
               ; result ()
            end
       | Ref_deref => (case dest (oneArg ()) of
                          Ref v => v
                        | Type _ => result ()
                        | _ => typeError ())
       | Ref_ref =>
            let
               val r = result ()
               val _ = 
                  case dest r of
                     Ref x => coerce {from = oneArg (), to = x} (* unify (oneArg (), x) *)
                   | Type _ => ()
                   | _ => typeError ()
            in
               r
            end
       | Vector_sub =>
            (case dest (#1 (twoArgs ())) of
                Vector x => x
              | Type _ => result ()
              | _ => typeError ())
       | Vector_vector =>
            let
                val r = result ()
                val _ =
                   case dest r of
                      Vector x => Vector.foreach (args, fn arg => coerce {from = arg, to = x})
                    | Type _ => ()
                    | _ => typeError ()
            in
               r
            end
       | Weak_get =>
            (case dest (oneArg ()) of
                Weak v => v
              | Type _ => result ()
              | _ => typeError ())
       | Weak_new =>
            let
               val r = result ()
               val _ =
                  case dest r of
                     Type _ => ()
                   | Weak x => coerce {from = oneArg (), to = x}
                   | _ => typeError ()
            in
               r
            end
       | _ => result ()
   end

end
