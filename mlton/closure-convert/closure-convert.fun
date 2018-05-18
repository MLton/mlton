(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * All local variables in the Sxml are renamed to new variables in Ssa,
 * unless they are global, as determined by the Globalization pass.
 * Renaming must happen because an Sxml variable will be bound in the Ssa
 * once for each lambda it occurs in.  The main trickiness is caused because a
 * property is used to implement the renamer map.  Hence, a variable binding
 * must always be visited with "newVar" or "newScope" before it is looked up
 * with "getNewVar".  "newScope" also handles resetting the variable to its
 * old value once the processing of the lambda is done.
 *)
functor ClosureConvert (S: CLOSURE_CONVERT_STRUCTS): CLOSURE_CONVERT = 
struct

open S

structure Globalize = Globalize (open Sxml)

local open Sxml
in
   structure Scases = Cases
   structure Sexp = Exp
   structure Sdec = Dec
   structure Slambda = Lambda
   structure Spat = Pat
   structure SprimExp = PrimExp
   structure SvarExp = VarExp
   structure Stype = Type
   open Atoms
end

local
   open Ssa
in
   structure Block = Block
   structure Datatype = Datatype
   structure Dexp = DirectExp
   structure Func = Func
   structure Function = Function
   structure SourceInfo = SourceInfo
   structure Type = Type
end

structure Value = AbstractValue (structure Ssa = Ssa
                                 structure Sxml = Sxml)
local open Value
in structure Lambdas = Lambdas
end

(* Accum.t is one of the results returned internally by the converter -- an
 * accumulation of toplevel Ssa globals and function declarations.
 *)
structure Accum =
   struct
      structure AL = AppendList

      datatype t = T of {globals: {var: Var.t,
                                   ty: Type.t,
                                   exp: Dexp.t} AL.t,
                         functions: Function.t list}

      val empty = T {globals = AL.empty, functions = []}

      fun addGlobals (T {globals, functions}, gs) =
         T {globals = AL.append (globals, AL.fromList gs),
            functions = functions}

      fun addGlobal (ac, g) = addGlobals (ac, [g])

      fun addFunc (T {globals, functions}, f) =
         T {globals = globals, functions = f :: functions}

      fun done (T {globals, functions}) =
         {functions = functions,
          globals =
          let
             (* Must shrink because coercions may be inserted at constructor
              * applications.  I'm pretty sure the shrinking will eliminate
              * any case expressions/local functions.
              * We must rebind eliminated variables because the shrinker is 
              * just processing globals and hence cannot safely delete a 
              * variable that has no occurrences, since there may still be 
              * occurrences in functions.
              *)
             val globals = AL.toList globals
             val vars = Vector.fromListMap (globals, #var)
             val tys = Vector.fromListMap (globals, #ty)
             val (start, blocks) =
                Dexp.linearize
                (Dexp.lett
                 {decs = List.map (globals, fn {var, exp, ...} =>
                                   {var = var, exp = exp}),
                  body = Dexp.tuple {exps = (Vector.fromListMap
                                             (globals, fn {var, ty, ...} =>
                                              Dexp.var (var, ty))),
                                     ty = Type.tuple tys}},
                 Ssa.Handler.Caller)
             val {blocks, ...} =
                Function.dest
                (Ssa.shrinkFunction
                 {globals = Vector.new0 ()}
                 (Function.new {args = Vector.new0 (),
                                blocks = Vector.fromList blocks,
                                mayInline = false, (* doesn't matter *)
                                name = Func.newNoname (),
                                raises = NONE,
                                returns = SOME (Vector.new1 (Type.tuple tys)),
                                start = start}))
          in
             if 1 <> Vector.length blocks
                then Error.bug (concat ["ClosureConvert.Accum.done: ",
                                        "shrinker didn't completely simplify"])
             else
                let
                   val ss = Block.statements (Vector.first blocks)
                   val vs = 
                      case Ssa.Statement.exp (Vector.last ss) of
                         Ssa.Exp.Tuple vs =>
                            if Vector.length vars = Vector.length vs
                               then vs
                            else Error.bug (concat ["ClosureConvert.Accum.done: ",
                                                    "shrinker didn't simplify right"])
                       | _ => Error.bug (concat ["ClosureConvert.Accum.done: ",
                                                 "shrinker didn't produce tuple"])
                   val ss = Vector.dropSuffix (ss, 1)
                   val rebinds =
                      Vector.keepAllMapi
                      (vs, fn (i, v) =>
                       if Var.equals (v, Vector.sub (vars, i))
                          then NONE
                          else SOME (Ssa.Statement.T 
                                     {exp = Ssa.Exp.Var v,
                                      ty = Vector.sub (tys, i),
                                      var = SOME (Vector.sub (vars, i))}))
                in
                   Vector.concat [ss, rebinds]
                end
          end}
   end

(*
val traceConvertExp =
   Trace.trace2 
   ("ClosureConvert.convertExp", 
    Sexp.layout, Instance.layout, Dexp.layout)
*)

val convertPrimExpInfo = Trace.info "ClosureConvert.convertPrimExp"
val valueTypeInfo = Trace.info "ClosureConvert.valueType"

structure LambdaFree = LambdaFree (Sxml)

local
   open LambdaFree
in
   structure Status = Status
end

structure LambdaInfo =
   struct
      datatype t =
         T of {
               con: Con.t ref,
               frees: Var.t vector ref,
               (* name is the original name in the source (i.e. SXML) program,
                * so the closure conversion output has some readability.
                *)
               name: Func.t,
               recs: Var.t vector ref,
               (* The type of its environment record. *)
               ty: Type.t option ref
               }

      fun frees (T {frees, ...}) = !frees
   end

structure VarInfo =
   struct
      type t = {frees: Var.t list ref ref,
                isGlobal: bool ref,
                lambda: Slambda.t option,
                replacement: Var.t ref,
                status: Status.t ref,
                value: Value.t}

      local
         fun make sel (r: t) = sel r
      in
         val lambda = valOf o make #lambda
         val value = make #value
      end
   end

val traceLoopBind =
   Trace.trace
   ("ClosureConvert.loopBind",
    fn {exp, ty = _: Stype.t, var} =>
    Layout.record [("var", Var.layout var),
                   ("exp", SprimExp.layout exp)],
    Unit.layout)

fun closureConvert
   (program as Sxml.Program.T {datatypes, body, overflow}): Ssa.Program.t =
   let
      val {get = conArg: Con.t -> Value.t option, set = setConArg, ...} =
         Property.getSetOnce (Con.plist,
                              Property.initRaise ("conArg", Con.layout))
      val {get = varInfo: Var.t -> VarInfo.t, set = setVarInfo, ...} =
         Property.getSetOnce
         (Var.plist, Property.initRaise ("closure convert info", Var.layout))
      val varInfo =
         Trace.trace 
         ("ClosureConvert.varInfo", Var.layout, Layout.ignore)
         varInfo
      val varExpInfo = varInfo o SvarExp.var
      val isGlobal = ! o #isGlobal o varInfo
      val isGlobal =
         Trace.trace 
         ("ClosureConvert.isGlobal", Var.layout, Bool.layout)
         isGlobal
      val value = #value o varInfo
      val varExp = value o SvarExp.var
      val expValue = varExp o Sexp.result
      (* ---------------------------------- *)
      (*             lambdaInfo             *)
      (* ---------------------------------- *)
      val {get = lambdaInfo: Slambda.t -> LambdaInfo.t,
           set = setLambdaInfo, ...} =
         Property.getSetOnce
         (Slambda.plist,
          Property.initRaise ("closure convert info", Layout.ignore))
      val allLambdas: Slambda.t list ref = ref []
      (* Do the flow analysis.
       * Initialize lambdaInfo and varInfo.
       *)
      val _ =
         Vector.foreach
         (datatypes, fn {cons, ...} =>
          Vector.foreach
          (cons, fn {con, arg} =>
           setConArg (con, (case arg of
                               NONE => NONE
                             | SOME t => SOME (Value.fromType t)))))
      val _ =
         let
            open Sxml
            val bogusFrees = ref []
            fun newVar' (x, v, lambda) =
               setVarInfo (x, {frees = ref bogusFrees,
                               isGlobal = ref false,
                               lambda = lambda,
                               replacement = ref x,
                               status = ref Status.init,
                               value = v})
            fun newVar (x, v) = newVar' (x, v, NONE)
            val newVar =
               Trace.trace2 
               ("ClosureConvert.newVar",
                Var.layout, Layout.ignore, Unit.layout)
               newVar
            fun varExps xs = Vector.map (xs, varExp)
            fun loopExp (e: Exp.t): Value.t =
               let
                  val {decs, result} = Exp.dest e
                  val () = List.foreach (decs, loopDec)
               in
                  varExp result
               end
            and loopDec (d: Dec.t): unit =
               let
                  datatype z = datatype Dec.t
               in
                  case d of
                     Fun {decs, ...} =>
                        (Vector.foreach (decs, fn {var, lambda, ty, ...} =>
                                         newVar' (var, Value.fromType ty,
                                                  SOME lambda))
                         ; (Vector.foreach
                            (decs, fn {var, lambda, ...} =>
                             Value.unify (value var,
                                          loopLambda (lambda, var)))))
                   | MonoVal b => loopBind b
                   | _ => Error.bug "ClosureConvert.loopDec: strange dec"
               end
            and loopBind arg =
               traceLoopBind
               (fn {var, ty, exp} =>
               let
                  fun set v = newVar (var, v)
                  fun new () =
                     let val v = Value.fromType ty
                     in set v; v
                     end
                  val new' = ignore o new
                  datatype z = datatype PrimExp.t
               in
                  case exp of
                     App {func, arg} =>
                        let val arg = varExp arg
                           val result = new ()
                        in Value.addHandler
                           (varExp func, fn l =>
                            let
                               val lambda = Value.Lambda.dest l
                               val {arg = formal, body, ...} =
                                  Lambda.dest lambda
                            in Value.coerce {from = arg,
                                             to = value formal}
                               ; Value.coerce {from = expValue body,
                                               to = result}
                            end)
                        end
                   | Case {cases, default, ...} =>
                        let
                           val result = new ()
                           fun branch e =
                              Value.coerce {from = loopExp e, to = result}
                           fun handlePat (Pat.T {con, arg, ...}) =
                              case (arg,      conArg con) of
                                 (NONE,        NONE)       => ()
                               | (SOME (x, _), SOME v)     => newVar (x, v)
                               | _ => Error.bug "ClosureConvert.loopBind: Case"
                           val _ = Cases.foreach' (cases, branch, handlePat)
                           val _ = Option.app (default, branch o #1)
                        in ()
                        end
                   | ConApp {con, arg, ...} =>
                        (case (arg,    conArg con) of
                            (NONE,   NONE)       => ()
                          | (SOME x, SOME v)     =>
                               Value.coerce {from = varExp x, to = v}
                          | _ => Error.bug "ClosureConvert.loopBind: ConApp"
                         ; new' ())
                   | Const _ => new' ()
                   | Handle {try, catch = (x, t), handler} =>
                        let
                           val result = new ()
                        in Value.coerce {from = loopExp try, to = result}
                           ; newVar (x, Value.fromType t)
                           ; Value.coerce {from = loopExp handler, to = result}
                        end
                   | Lambda l => set (loopLambda (l, var))
                   | PrimApp {prim, args, ...} =>
                        set (Value.primApply {prim = prim,
                                              args = varExps args,
                                              resultTy = ty})
                   | Profile _ => new' ()
                   | Raise _ => new' ()
                   | Select {tuple, offset} =>
                        set (Value.select (varExp tuple, offset))
                   | Tuple xs =>
                        if Value.typeIsFirstOrder ty
                           then new' ()
                      else set (Value.tuple (Vector.map (xs, varExp)))
                   | Var x => set (varExp x)
               end) arg
            and loopLambda (lambda: Lambda.t, x: Var.t): Value.t =
               let
                  val _ = List.push (allLambdas, lambda)
                  val {arg, argType, body, ...} = Lambda.dest lambda
                  val _ =
                     setLambdaInfo
                     (lambda,
                      LambdaInfo.T {con = ref Con.bogus,
                                    frees = ref (Vector.new0 ()),
                                    name = Func.newString (Var.originalName x),
                                    recs = ref (Vector.new0 ()),
                                    ty = ref NONE})
                  val _ = newVar (arg, Value.fromType argType)
               in
                  Value.lambda (lambda,
                                Type.arrow (argType, Value.ty (loopExp body)))
               end
            val _ =
               Control.trace (Control.Pass, "flow analysis")
               loopExp body
         in ()
         end
      val _ =
         Control.diagnostics
         (fn display =>
          Sexp.foreachBoundVar
          (body, fn (x, _, _) => display (let open Layout
                                          in seq [Var.layout x,
                                                  str " ",
                                                  Value.layout (value x)]
                                          end)))
      val overflow = valOf overflow
      val _ =
         Control.trace (Control.Pass, "free variables")
         LambdaFree.lambdaFree
         {program = program,
          overflow = overflow,
          varInfo = fn x => let val {frees, status, ...} = varInfo x
                            in {frees = frees, status = status}
                            end,
          lambdaInfo = fn l => let val LambdaInfo.T {frees, recs, ...} = lambdaInfo l
                               in {frees = frees, recs = recs}
                               end}
      val _ =
         if !Control.closureConvertGlobalize
            then Control.trace (Control.Pass, "globalize")
                 Globalize.globalize {program = program,
                                      lambdaFree = LambdaInfo.frees o lambdaInfo,
                                      varGlobal = #isGlobal o varInfo}
         else ()
      local
         fun removeGlobal v = Vector.keepAll (v, not o isGlobal)
         val _ =
            List.foreach (!allLambdas, fn l =>
                          let
                             val LambdaInfo.T {frees, recs, ...} = lambdaInfo l
                          in
                             frees := removeGlobal (!frees)
                             ; recs := removeGlobal (!recs)
                          end)
      in
      end
      val {get = lambdasInfoOpt, ...} =
         Property.get (Lambdas.plist, Property.initFun (fn _ => ref NONE))
      val (convertType, destroyConvertType) =
         let
            val {get, set, destroy, ...} =
               Property.destGetSetOnce (Tycon.plist, Property.initConst NONE)

            fun nullary c v =
               if Vector.isEmpty v
                  then c
               else Error.bug "ClosureConvert.convertType.nullary: bogus application of nullary tycon"

            fun unary make v =
               if 1 = Vector.length v
                  then make (Vector.first v)
               else Error.bug "ClosureConvert.convertType.unary: bogus application of unary tycon"
            val tycons =
               [(Tycon.arrow, fn _ => Error.bug "ClosureConvert.convertType.array"),
                (Tycon.array, unary Type.array),
                (Tycon.cpointer, nullary Type.cpointer),
                (Tycon.intInf, nullary Type.intInf),
                (Tycon.reff, unary Type.reff),
                (Tycon.thread, nullary Type.thread),
                (Tycon.tuple, Type.tuple),
                (Tycon.vector, unary Type.vector),
                (Tycon.weak, unary Type.weak)]
               @ Vector.toListMap (Tycon.reals, fn (t, s) => (t, nullary (Type.real s)))
               @ Vector.toListMap (Tycon.words, fn (t, s) => (t, nullary (Type.word s)))

            val _ = List.foreach (tycons, fn (tycon, f) => set (tycon, SOME f))

            val {hom = convertType, destroy = destroyConvertType} =
               Stype.makeMonoHom
               {con = fn (_, tycon, ts) =>
                case get tycon of
                   NONE => nullary (Type.datatypee tycon) ts
                 | SOME f => f ts}
         in
            (convertType,
             fn () => (destroy () ; destroyConvertType ()))
         end
      (* newDatatypes accumulates the new datatypes built for sets of lambdas. *)
      val newDatatypes: Datatype.t list ref = ref []
      fun valueType arg: Type.t =
         Trace.traceInfo (valueTypeInfo,
                          Layout.ignore,
                          Type.layout,
                          Trace.assertTrue)
         (fn (v: Value.t) =>
         let
            val r = Value.ssaType v
         in
            case !r of
               SOME t => t
             | NONE =>
                  let
                     val t = 
                        case Value.dest v of
                           Value.Array v => Type.array (valueType v)
                         | Value.Lambdas ls => #ty (lambdasInfo ls)
                         | Value.Ref v => Type.reff (valueType v)
                         | Value.Type t => convertType t
                         | Value.Tuple vs =>
                              Type.tuple (Vector.map (vs, valueType))
                         | Value.Vector v => Type.vector (valueType v)
                         | Value.Weak v => Type.weak (valueType v)
                  in r := SOME t; t
                  end
         end) arg
      and lambdasInfo (ls: Lambdas.t): {cons: {lambda: Slambda.t,
                                               con: Con.t} vector,
                                        ty: Type.t} =
         let
            val r = lambdasInfoOpt ls
         in
            case !r of
               SOME info => info
             | NONE => 
                  let
                     val tycon = Tycon.newString "lambdas"
                     val cons =
                        Vector.fromListMap
                        (Lambdas.toList ls, fn l =>
                         {lambda = Value.Lambda.dest l,
                          con = Con.newString "Env"})
                     val ty = Type.datatypee tycon
                     val info = {ty = ty, cons = cons}
                     val _ = r := SOME info
                     (* r must be set before the following, because calls to
                      * lambdaInfoType may refer to the type of this lambdasInfo.
                      *)
                     val cons =
                        Vector.map
                        (cons, fn {con, lambda} =>
                         {con = con,
                          args = Vector.new1 (lambdaInfoType
                                              (lambdaInfo lambda))})
                     val _ = List.push (newDatatypes,
                                        Datatype.T {tycon = tycon,
                                                    cons = cons})
                  in
                     info
                  end
         end
      and varInfoType ({value, ...}: VarInfo.t) = valueType value
      and lambdaInfoType (LambdaInfo.T {frees, ty, ...}): Type.t =
         case !ty of
            NONE =>
               let val t = Type.tuple (Vector.map
                                        (!frees, varInfoType o varInfo))
               in ty := SOME t; t
               end
          | SOME t => t
      fun valueLambdasInfo v =
         case Value.dest v of
            Value.Lambdas l => lambdasInfo l
          | _ => Error.bug "ClosureConvert.valueLambdasInfo: non-lambda"
      val varLambdasInfo = valueLambdasInfo o value
      val emptyTypes = Vector.new0 ()
      val datatypes =
         Vector.map
         (datatypes, fn {tycon, cons, ...} =>
          Datatype.T
          {tycon = tycon,
           cons = (Vector.map
                   (cons, fn {con, ...} =>
                    {con = con,
                     args = (case conArg con of
                                NONE => emptyTypes
                              | SOME v => Vector.new1 (valueType v))}))})
      (* Variable renaming *)
      fun newVarInfo (x: Var.t, {isGlobal, replacement, ...}: VarInfo.t): Var.t =
         if !isGlobal
            then x
         else let val x' = Var.new x
              in replacement := x'; x'
              end
      fun newVar x = newVarInfo (x, varInfo x)
      val newVar = Trace.trace ("ClosureConvert.newVar", Var.layout, Var.layout) newVar
      fun newScope (xs: Var.t vector, f: Var.t vector -> 'a): 'a =
         let
            val old = Vector.map (xs, ! o #replacement o varInfo)
            val res = f (Vector.map (xs, newVar))
            val _ = Vector.foreach2 (xs, old, fn (x, x') =>
                                     #replacement (varInfo x) := x')
         in
            res
         end
      (*------------------------------------*)
      (*               coerce               *)
      (*------------------------------------*)
      val traceCoerce =
         Trace.trace3 
         ("ClosureConvert.coerce", 
          Dexp.layout, Value.layout, Value.layout, Dexp.layout)
      (*       val traceCoerceTuple =
       *         let val layoutValues = List.layout (", ", Value.layout)
       *         in Trace.trace3 ("ClosureConvert.coerceTuple", Dexp.layout,
       *                         layoutValues, layoutValues, Dexp.layout)
       *         end
       *)
      fun coerce arg: Dexp.t =
         traceCoerce
         (fn (e: Dexp.t, from: Value.t, to: Value.t) =>
          if Value.equals (from, to)
             then e
          else 
             case (Value.dest from, Value.dest to) of
                (Value.Tuple vs, Value.Tuple vs') =>
                   coerceTuple (e, valueType from, vs, valueType to, vs')
              | (Value.Lambdas ls, Value.Lambdas ls') =>
                   if Lambdas.equals (ls, ls')
                      then e
                   else
                      let
                         val {cons, ...} = lambdasInfo ls
                         val {cons = cons', ty, ...} = lambdasInfo ls'
                         val _ =
                            Vector.foreach
                            (cons', fn {lambda, con, ...} =>
                             let
                                val LambdaInfo.T {con = r, ...} =
                                   lambdaInfo lambda
                             in r := con
                             end)
                         val exp =
                            Dexp.casee
                            {test = e,
                             default = NONE,
                             ty = ty,
                             cases =
                             Dexp.Con
                             (Vector.map
                              (cons, fn {lambda, con} =>
                               let
                                  val info as LambdaInfo.T {con = r, ...} =
                                     lambdaInfo lambda
                                  val tuple = (Var.newNoname (),
                                               lambdaInfoType info)
                               in {con = con,
                                   args = Vector.new1 tuple,
                                   body = (Dexp.conApp
                                           {con = !r,
                                            ty = ty,
                                            args =
                                            Vector.new1 (Dexp.var tuple)})}
                               end))}
                      in exp
                      end
              | _ => Error.bug "ClosureConvert.coerce") arg
      and coerceTuple arg =
         (*      traceCoerceTuple *)
         (fn (e: Dexp.t,
              ty: Type.t, vs: Value.t vector,
              ty': Type.t, vs': Value.t vector) =>
          if Type.equals (ty, ty')
             then e
          else 
             Dexp.detuple
             {tuple = e,
              length = Vector.length vs,
              body =
              fn components => 
              Dexp.tuple
              {exps = Vector.map3 (components, vs, vs',
                                   fn (x, v, v') =>
                                   coerce (Dexp.var (x, valueType v), v, v')),
               ty = ty'}}) arg
      fun convertVarInfo (info as {replacement, ...}: VarInfo.t) =
         Dexp.var (!replacement, varInfoType info)
      val convertVar = convertVarInfo o varInfo
      val convertVarExp = convertVar o SvarExp.var
      val handlesSignals =
         Sexp.hasPrim (body, fn p =>
                       case Prim.name p of
                          Prim.Name.MLton_installSignalHandler => true
                        | _ => false)
      (*------------------------------------*)                 
      (*               apply                *)
      (*------------------------------------*)
      fun apply {func, arg, resultVal}: Dexp.t =
         let
            val func = varExpInfo func
            val arg = varExpInfo arg
            val funcVal = VarInfo.value func
            val argVal = VarInfo.value arg
            val argExp = convertVarInfo arg
            val ty = valueType resultVal
            val {cons, ...} = valueLambdasInfo funcVal
         in Dexp.casee
            {test = convertVarInfo func,
             ty = ty,
             default = NONE,
             cases =
             Dexp.Con
             (Vector.map
              (cons, fn {lambda, con} =>
               let
                  val {arg = param, body, ...} = Slambda.dest lambda
                  val info as LambdaInfo.T {name, ...} = lambdaInfo lambda
                  val result = expValue body
                  val env = (Var.newString "env", lambdaInfoType info)
               in {con = con,
                   args = Vector.new1 env,
                   body = coerce (Dexp.call
                                  {func = name,
                                   args = Vector.new2 (Dexp.var env,
                                                       coerce (argExp, argVal,
                                                               value param)),
                                   ty = valueType result},
                                  result, resultVal)}
               end))}
         end
      (*------------------------------------*)
      (*             convertExp             *)
      (*------------------------------------*)
      fun lambdaInfoTuple (info as LambdaInfo.T {frees, ...}): Dexp.t =
         Dexp.tuple {exps = Vector.map (!frees, convertVar),
                     ty = lambdaInfoType info}
      fun recursives (old: Var.t vector, new: Var.t vector, env) =
         Vector.fold2
         (old, new, [], fn (old, new, ac) =>
          let
             val {cons, ty, ...} = varLambdasInfo old
             val l = VarInfo.lambda (varInfo old)
          in
             case Vector.peek (cons, fn {lambda = l', ...} =>
                               Slambda.equals (l, l')) of
                NONE => Error.bug "ClosureConvert.recursives: lambda must exist in its own set"
              | SOME {con, ...} =>
                   {var = new,
                    ty = ty,
                    exp = Dexp.conApp {con = con, ty = ty,
                                       args = Vector.new1 (Dexp.var env)}}
                   :: ac
          end)
      val recursives =
         Trace.trace ("ClosureConvert.recursives",
                      fn (a, b, _) =>
                      Layout.tuple [Vector.layout Var.layout a,
                                    Vector.layout Var.layout b],
                      Layout.ignore)
         recursives
      val raises: Type.t vector option =
         let
            exception Yes of Type.t vector
         in
            (Sexp.foreachPrimExp
             (body, fn (_, _, e) =>
              case e of
                 SprimExp.Handle {catch = (x, _), ...} =>
                    raise (Yes (Vector.new1 (varInfoType (varInfo x))))
               | _ => ())
             ; NONE)
            handle Yes ts => SOME ts
         end
      val shrinkFunction =
         if !Control.closureConvertShrink
            then Ssa.shrinkFunction {globals = Vector.new0 ()}
         else fn f => f
      fun addFunc (ac, {args, body, isMain, mayInline, name, returns}) =
         let
            val (start, blocks) =
               Dexp.linearize (body, Ssa.Handler.Caller)
            val f =
               shrinkFunction
               (Function.new {args = args,
                              blocks = Vector.fromList blocks,
                              mayInline = mayInline,
                              name = name,
                              raises = if isMain
                                          then NONE
                                          else raises,
                              returns = SOME returns,
                              start = start})
            val f =
               if isMain
                  then Function.profile (f, SourceInfo.main)
               else f
         in
            Accum.addFunc (ac, f)
         end
      (* Closure convert an expression, returning:
       *   - the target ssa expression
       *   - a list of global declarations (in order)
       *   - a list of function declarations
       * Accumulate the globals onto the end of the given ones.
       *)
      fun convertExp (e: Sexp.t, ac: Accum.t): Dexp.t * Accum.t =
         let
            val {decs, result} = Sexp.dest e
            (* Process decs left to right, since bindings of variables
             * must be visited before uses.
             *)
            val (decs, ac) =
               List.fold
               (decs, ([], ac), fn (d, (binds, ac)) =>
                case d of
                   Sdec.MonoVal {exp, var, ...} =>
                      let
                         val info as {isGlobal, value, ...} = varInfo var
                         val (exp, ac) = convertPrimExp (exp, value, ac)
                         val bind = {var = newVarInfo (var, info),
                                     ty = valueType value,
                                     exp = exp}
                      in if !isGlobal
                            then (binds, Accum.addGlobal (ac, bind))
                         else (bind :: binds, ac)
                      end
                 | Sdec.Fun {decs, ...} =>
                      if Vector.isEmpty decs
                         then (binds, ac)
                      else
                         let 
                            val {lambda, var, ...} = Vector.first decs
                            val info = lambdaInfo lambda
                            val tupleVar = Var.newString "tuple"
                            val tupleTy = lambdaInfoType info
                            val binds' =
                               {var = tupleVar,
                                ty = tupleTy,
                                exp = lambdaInfoTuple info}
                               :: (recursives
                                   (Vector.map (decs, #var),
                                    Vector.map (decs, newVar o #var),
                                    (tupleVar, tupleTy)))
                            val (binds, ac) =
                               if isGlobal var
                                  then (binds, Accum.addGlobals (ac, binds'))
                               else (List.fold (binds', binds, op ::), ac)
                         in (binds,
                             Vector.fold (decs, ac, fn ({lambda, ...}, ac) =>
                                          convertLambda (lambda,
                                                         lambdaInfo lambda,
                                                         ac)))
                         end
                 | _ => Error.bug "ClosureConvert.convertExp: strange dec")
         in (Dexp.lett {decs = List.fold (decs, [], fn ({var, exp, ...}, ac) =>
                                          {var = var, exp = exp} :: ac),
                        body = convertVarExp result},
             ac)
         end
      and convertPrimExp arg : Dexp.t * Accum.t =
         Trace.traceInfo (convertPrimExpInfo,
                          SprimExp.layout o #1,
                          Layout.ignore,
                          Trace.assertTrue)
         (fn (e: SprimExp.t, v: Value.t, ac: Accum.t) =>
         let
            val ty = valueType v
            fun convertJoin (e, ac) =
               let val (e', ac) = convertExp (e, ac)
               in (coerce (e', expValue e, v), ac)
               end
            fun simple e = (e, ac)
         in
            case e of
               SprimExp.App {func, arg} =>
                  (apply {func = func, arg = arg, resultVal = v},
                   ac)
             | SprimExp.Case {test, cases, default} =>
                  let
                     val (default, ac) =
                        case default of
                           NONE => (NONE, ac)
                         | SOME (e, _) => let
                                             val (e, ac) =  convertJoin (e, ac)
                                          in
                                             (SOME e, ac)
                                          end
                     fun doCases (cases, finish, make) =
                        let
                           val (cases, ac) =
                              Vector.mapAndFold
                              (cases, ac, fn ((x, e), ac) =>
                               let
                                  val make = make x
                                  val (body, ac) = convertJoin (e, ac)
                               in (make body, ac)
                               end)
                        in (finish cases, ac)
                        end
                     val (cases, ac) =
                        case cases of
                           Scases.Con cases =>
                              doCases
                              (cases, Dexp.Con,
                               fn Spat.T {con, arg, ...} =>
                               let
                                  val args =
                                     case (conArg con, arg) of
                                        (NONE, NONE) => Vector.new0 ()
                                      | (SOME v, SOME (arg, _)) =>
                                           Vector.new1 (newVar arg, valueType v)
                                      | _ => Error.bug "ClosureConvert.convertPrimExp: Case,constructor mismatch"
                               in
                                  fn body => {args = args,
                                              body = body,
                                              con = con}
                               end)
                         | Scases.Word (s, cs) =>
                              doCases (cs, fn cs => Dexp.Word (s, cs),
                                       fn i => fn e => (i, e))
                  in (Dexp.casee
                      {test = convertVarExp test,
                       ty = ty, cases = cases, default = default},
                      ac)
                  end
             | SprimExp.ConApp {con = con, arg, ...} =>
                  simple
                  (Dexp.conApp
                   {con = con,
                    ty = ty,
                    args = (case (arg, conArg con) of
                               (NONE, NONE) => Vector.new0 ()
                             | (SOME arg, SOME conArg) =>
                                  let
                                     val arg = varExpInfo arg
                                     val argVal = VarInfo.value arg
                                     val arg = convertVarInfo arg
                                  in if Value.equals (argVal, conArg)
                                        then Vector.new1 arg
                                     else Vector.new1 (coerce (arg, argVal, conArg))
                                  end
                             | _ => Error.bug "ClosureConvert.convertPrimExp: ConApp,constructor mismatch")})
             | SprimExp.Const c => simple (Dexp.const c)
             | SprimExp.Handle {try, catch = (catch, _), handler} =>
                  let
                     val catchInfo = varInfo catch
                     val (try, ac) = convertJoin (try, ac)
                     val catch = (newVarInfo (catch, catchInfo),
                                  varInfoType catchInfo)
                     val (handler, ac) = convertJoin (handler, ac)
                  in (Dexp.handlee {try = try, ty = ty,
                                    catch = catch, handler = handler},
                      ac)
                  end
             | SprimExp.Lambda l =>
                  let
                     val info = lambdaInfo l
                     val ac = convertLambda (l, info, ac)
                     val {cons, ...} = valueLambdasInfo v
                  in case Vector.peek (cons, fn {lambda = l', ...} =>
                                       Slambda.equals (l, l')) of
                     NONE => Error.bug "ClosureConvert.convertPrimExp: Lambda,lambda must exist in its own set"
                   | SOME {con, ...} =>
                        (Dexp.conApp {con = con, ty = ty,
                                      args = Vector.new1 (lambdaInfoTuple info)},
                         ac)
                  end
             | SprimExp.PrimApp {prim, targs, args} =>
                  let
                     val prim = Prim.map (prim, convertType)
                     open Prim.Name
                     fun arg i = Vector.sub (args, i)
                     val v1 = Vector.new1
                     val v2 = Vector.new2
                     val v3 = Vector.new3
                     fun primApp (targs, args) =
                        Dexp.primApp {args = args,
                                      prim = prim,
                                      targs = targs,
                                      ty = ty}
                  in
                     if Prim.mayOverflow prim
                        then simple (Dexp.arith
                                     {args = Vector.map (args, convertVarExp),
                                      overflow = Dexp.raisee (convertVar overflow),
                                      prim = prim,
                                      ty = ty})
                     else
                        let
                           datatype z = datatype Prim.Name.t
                        in
                           simple
                           (case Prim.name prim of
                               Array_update =>
                                  let
                                     val a = varExpInfo (arg 0)
                                     val y = varExpInfo (arg 2)
                                     val v = Value.deArray (VarInfo.value a)
                                  in
                                     primApp (v1 (valueType v),
                                              v3 (convertVarInfo a,
                                                  convertVarExp (arg 1),
                                                  coerce (convertVarInfo y,
                                                          VarInfo.value y, v)))
                                  end
                             | MLton_eq =>
                                  let
                                     val a0 = varExpInfo (arg 0)
                                     val a1 = varExpInfo (arg 1)
                                     fun doit () =
                                        primApp (v1 (valueType (VarInfo.value a0)),
                                                 v2 (convertVarInfo a0,
                                                     convertVarInfo a1))
                                  in
                                     case (Value.dest (VarInfo.value a0),
                                           Value.dest (VarInfo.value a1)) of
                                        (Value.Lambdas l, Value.Lambdas l') =>
                                           if Lambdas.equals (l, l')
                                              then doit () 
                                           else Dexp.falsee
                                      | _ => doit ()
                                  end
                             | MLton_equal =>
                                  let
                                     val a0 = varExpInfo (arg 0)
                                     val a1 = varExpInfo (arg 1)
                                     fun doit () =
                                        primApp (v1 (valueType (VarInfo.value a0)),
                                                 v2 (convertVarInfo a0,
                                                     convertVarInfo a1))
                                  in
                                     case (Value.dest (VarInfo.value a0),
                                           Value.dest (VarInfo.value a1)) of
                                        (Value.Lambdas l, Value.Lambdas l') =>
                                           if Lambdas.equals (l, l')
                                              then doit () 
                                           else Dexp.falsee
                                      | _ => doit ()
                                  end
                             | MLton_handlesSignals =>
                                  if handlesSignals
                                     then Dexp.truee
                                  else Dexp.falsee
                             | Ref_assign =>
                                  let
                                     val r = varExpInfo (arg 0)
                                     val y = varExpInfo (arg 1)
                                     val v = Value.deRef (VarInfo.value r)
                                  in
                                     primApp (v1 (valueType v),
                                              v2 (convertVarInfo r,
                                                  coerce (convertVarInfo y,
                                                          VarInfo.value y, v)))
                                  end
                             | Ref_ref =>
                                  let
                                     val y = varExpInfo (arg 0)
                                     val v = Value.deRef v
                                  in
                                     primApp (v1 (valueType v),
                                              v1 (coerce (convertVarInfo y,
                                                          VarInfo.value y, v)))
                                  end
                             | MLton_serialize =>
                                  let
                                     val y = varExpInfo (arg 0)
                                     val v =
                                        Value.serialValue (Vector.first targs)
                                  in
                                     primApp (v1 (valueType v),
                                              v1 (coerce (convertVarInfo y,
                                                          VarInfo.value y, v)))
                                  end
                             | Vector_vector =>
                                  let
                                     val ys = Vector.map (args, varExpInfo)
                                     val v = Value.deVector v
                                  in
                                     primApp (v1 (valueType v),
                                              Vector.map (ys, fn y =>
                                                          coerce (convertVarInfo y,
                                                                  VarInfo.value y, v)))
                                  end
                             | Weak_new =>
                                  let
                                     val y = varExpInfo (arg 0)
                                     val v = Value.deWeak v
                                  in
                                     primApp (v1 (valueType v),
                                              v1 (coerce (convertVarInfo y,
                                                          VarInfo.value y, v)))
                                  end
                             | _ =>
                                  let
                                     val args = Vector.map (args, varExpInfo)
                                  in
                                     primApp
                                     (Prim.extractTargs
                                      (prim,
                                       {args = Vector.map (args, varInfoType),
                                        result = ty,
                                        typeOps = {deArray = Type.deArray,
                                                   deArrow = fn _ => Error.bug "ClosureConvert.convertPrimExp: deArrow",
                                                   deRef = Type.deRef,
                                                   deVector = Type.deVector,
                                                   deWeak = Type.deWeak}}),
                                       Vector.map (args, convertVarInfo))
                                  end)
                        end
                  end
             | SprimExp.Profile e => simple (Dexp.profile e)
             | SprimExp.Raise {exn, ...} =>
                  simple (Dexp.raisee (convertVarExp exn))
             | SprimExp.Select {offset, tuple} =>
                  simple (Dexp.select {offset = offset,
                                       tuple = convertVarExp tuple,
                                       ty = ty})
             | SprimExp.Tuple xs =>
                  simple (Dexp.tuple {exps = Vector.map (xs, convertVarExp),
                                      ty = ty})
             | SprimExp.Var y => simple (convertVarExp y)
         end) arg
      and convertLambda (lambda: Slambda.t,
                         info as LambdaInfo.T {frees, name, recs, ...},
                         ac: Accum.t): Accum.t =
         let
            val {arg = argVar, body, mayInline, ...} = Slambda.dest lambda
            val argVarInfo = varInfo argVar
            val env = Var.newString "env"
            val envType = lambdaInfoType info
            val args = Vector.new2 ((env, envType),
                                    (newVarInfo (argVar, argVarInfo),
                                     varInfoType argVarInfo))
            val returns = Vector.new1 (valueType (expValue body))
            val recs = !recs
         in
            newScope
            (!frees, fn components =>
             newScope
             (recs, fn recs' =>
              let
                 val decs = recursives (recs, recs', (env, envType))
                 val (body, ac) = convertExp (body, ac)
                 val body =
                    Dexp.lett
                    {decs = List.fold (decs, [], fn ({var, exp, ...}, ac) =>
                                       {var = var, exp = exp} :: ac),
                     body = Dexp.detupleBind {tuple = env,
                                              tupleTy = envType,
                                              components = components,
                                              body = body}}
              in
                 addFunc (ac, {args = args,
                               body = body,
                               isMain = false,
                               mayInline = mayInline,
                               name = name,
                               returns = returns})
              end))
         end
      (*------------------------------------*)
      (*    main body of closure convert    *)
      (*------------------------------------*)
      val main = Func.newString "main"
      val {functions, globals} =
         Control.trace (Control.Pass, "convert")
         (fn () =>
          let
             val (body, ac) = convertExp (body, Accum.empty)
             val ac = addFunc (ac, {args = Vector.new0 (),
                                    body = body,
                                    mayInline = false,
                                    isMain = true,
                                    name = main,
                                    returns = Vector.new1 Type.unit})
          in Accum.done ac
          end) ()
      val datatypes = Vector.concat [datatypes, Vector.fromList (!newDatatypes)]
      val program =
         Ssa.Program.T {datatypes = datatypes,
                        globals = globals,
                        functions = functions,
                        main = main}
      val _ = destroyConvertType ()
      val _ = Value.destroy ()
      val _ = Ssa.Program.clear program
   in
      program
   end

end
