(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor RemoveUnused2 (S: SSA2_TRANSFORM_STRUCTS): SSA2_TRANSFORM =
struct

open S
open Exp Statement Transfer

structure Used =
   struct
      structure L = TwoPointLattice (val bottom = "unused"
                                     val top = "used")
      open L
      val use = makeTop
      val isUsed = isTop
      val whenUsed = addHandler
   end

structure Coned =
   struct
      structure L = TwoPointLattice (val bottom = "not coned"
                                     val top = "coned")
      open L
      val con = makeTop
      val isConed = isTop
      val whenConed = addHandler
   end

structure Deconed =
   struct
      structure L = TwoPointLattice (val bottom = "not deconed"
                                     val top = "deconed")
      open L
      val decon = makeTop
      val isDeconed = isTop
   end

structure MayReturn =
   struct
      structure L = TwoPointLattice (val bottom = "does not return"
                                     val top = "may return")
      open L
      val return = makeTop
      val mayReturn = isTop
      val whenReturns = addHandler
   end

structure MayRaise =
   struct
      structure L = TwoPointLattice (val bottom = "does not raise"
                                     val top = "may raise")
      open L
      val raisee = makeTop
      val mayRaise = isTop
      val whenRaises = addHandler
   end


structure VarInfo =
   struct
      datatype t = T of {ty: Type.t,
                         used: Used.t}

      fun layout (T {used, ...}) = Used.layout used

      local
         fun make f (T r) = f r
      in
         val ty = make #ty
         val used = make #used
      end

      fun new (ty : Type.t): t = T {ty = ty,
                                    used = Used.new ()}

      val use = Used.use o used
      val isUsed = Used.isUsed o used
      fun whenUsed (vi, th) = Used.whenUsed (used vi, th)
   end

structure ConInfo =
   struct
      datatype t = T of {args: (VarInfo.t * Type.t) Prod.t,
                         coned: Coned.t,
                         deconed: Deconed.t,
                         dummy: {con: Con.t, args: Type.t Prod.t,
                                 ty: Type.t, exp: Exp.t},
                         used: Used.t}

      fun layout (T {args, coned, deconed, used, ...}) =
         Layout.record [("args", Prod.layout (args, VarInfo.layout o #1)),
                        ("coned", Coned.layout coned),
                        ("deconed", Deconed.layout deconed),
                        ("used", Used.layout used)]

      local
         fun make f (T r) = f r
      in
         val args = make #args
         val coned = make #coned
         val deconed = make #deconed
         val dummy = make #dummy
         val used = make #used
      end

      val con = Coned.con o coned
      val isConed = Coned.isConed o coned
      fun whenConed (ci, th) = Coned.whenConed (coned ci, th)

      val decon = Deconed.decon o deconed
      val isDeconed = Deconed.isDeconed o deconed

      val use = Used.use o used
      val isUsed = Used.isUsed o used
      fun whenUsed (vi, th) = Used.whenUsed (used vi, th)

      fun new {args: Type.t Prod.t,
               dummy: {con: Con.t, args: Type.t Prod.t,
                       ty: Type.t, exp: Exp.t}}: t =
         T {args = Prod.map (args, fn ty => (VarInfo.new ty, ty)),
            coned = Coned.new (),
            deconed = Deconed.new (),
            dummy = dummy,
            used = Used.new ()}
   end

structure TyconInfo =
   struct
      datatype t = T of {cons: Con.t vector,
                         dummy: {con: Con.t, args: Type.t Prod.t},
                         numCons: int ref,
                         used: Used.t}

      fun layout (T {used, ...}) =
         Layout.record [("used", Used.layout used)]

      local
         fun make f (T r) = f r
         fun make' f = (make f, ! o (make f))
      in
         val cons = make #cons
         val dummy = make #dummy
         val (numCons', numCons) = make' #numCons
         val used = make #used
      end

      fun new {cons: Con.t vector,
               dummy: {con: Con.t, args: Type.t Prod.t}}: t =
         T {cons = cons,
            dummy = dummy,
            numCons = ref ~1,
            used = Used.new ()}
   end

structure TypeInfo =
   struct
      datatype t = T of {deconed: bool ref,
                         simplify: Type.t option ref,
                         used: bool ref}

      local
         fun make f (T r) = f r
         fun make' f = (make f, ! o (make f))
      in
         val (deconed', _) = make' #deconed
         val (simplify', _) = make' #simplify
         val (used', _) = make' #used
      end

      fun new (): t = T {deconed = ref false,
                         simplify = ref NONE,
                         used = ref false}
   end

structure FuncInfo =
   struct
      datatype t = T of {args: (VarInfo.t * Type.t) vector,
                         bugLabel: Label.t option ref,
                         mayRaise: MayRaise.t,
                         mayReturn: MayReturn.t,
                         raiseLabel: Label.t option ref,
                         raises: (VarInfo.t * Type.t) vector option,
                         returnLabel: Label.t option ref,
                         returns: (VarInfo.t * Type.t) vector option,
                         used: Used.t,
                         wrappers: Block.t list ref}

      fun layout (T {args,
                     mayRaise, mayReturn,
                     raises, returns,
                     used,
                     ...}) =
         Layout.record [("args", Vector.layout
                                 (Layout.tuple2 (VarInfo.layout, Type.layout))
                                 args),
                        ("mayRaise", MayRaise.layout mayRaise),
                        ("mayReturn", MayReturn.layout mayReturn),
                        ("raises", Option.layout
                                   (Vector.layout
                                    (Layout.tuple2 (VarInfo.layout, Type.layout)))
                                   raises),
                        ("returns", Option.layout
                                    (Vector.layout
                                     (Layout.tuple2 (VarInfo.layout, Type.layout)))
                                    returns),
                        ("used", Used.layout used)]

      local
         fun make f (T r) = f r
         fun make' f = (make f, ! o (make f))
      in
         val args = make #args
         val mayRaise' = make #mayRaise
         val mayReturn' = make #mayReturn
         val raiseLabel = make #raiseLabel
         val raises = make #raises
         val returnLabel = make #returnLabel
         val returns = make #returns
         val used = make #used
         val (wrappers', wrappers) = make' #wrappers
      end

      val raisee = MayRaise.raisee o mayRaise'
      val mayRaise = MayRaise.mayRaise o mayRaise'
      fun whenRaises (fi, th) = MayRaise.whenRaises (mayRaise' fi, th)
      fun flowRaises (fi, fi') = MayRaise.<= (mayRaise' fi, mayRaise' fi')

      val return = MayReturn.return o mayReturn'
      fun whenReturns (fi, th) = MayReturn.whenReturns (mayReturn' fi, th)
      val mayReturn = MayReturn.mayReturn o mayReturn'
      fun flowReturns (fi, fi') = MayReturn.<= (mayReturn' fi, mayReturn' fi')

      val use = Used.use o used
      val isUsed = Used.isUsed o used
      fun whenUsed (fi, th) = Used.whenUsed (used fi, th)

      fun new {args: (VarInfo.t * Type.t) vector,
               raises: (VarInfo.t * Type.t) vector option,
               returns: (VarInfo.t * Type.t) vector option}: t =
         T {args = args,
            bugLabel = ref NONE,
            mayRaise = MayRaise.new (),
            mayReturn = MayReturn.new (),
            raiseLabel = ref NONE,
            raises = raises,
            returnLabel = ref NONE,
            returns = returns,
            used = Used.new (),
            wrappers = ref []}
   end

structure LabelInfo =
   struct
      datatype t = T of {args: (VarInfo.t * Type.t) vector,
                         func: FuncInfo.t,
                         used: Used.t,
                         wrappers: (Type.t vector * Label.t) list ref}

      fun layout (T {args, used, ...}) =
         Layout.record [("args", Vector.layout (VarInfo.layout o #1) args),
                        ("used", Used.layout used)]

      fun new {args: (VarInfo.t * Type.t) vector, func: FuncInfo.t}: t =
         T {args = args,
            func = func,
            used = Used.new (),
            wrappers = ref []}

      local
         fun make f (T r) = f r
         fun make' f = (make f, ! o (make f))
      in
         val args = make #args
         val func = make #func
         val used = make #used
         val (wrappers', wrappers) = make' #wrappers
      end

      val use = Used.use o used
      val isUsed = Used.isUsed o used
      fun whenUsed (li, th) = Used.whenUsed (used li, th)
   end


fun transform2 (Program.T {datatypes, globals, functions, main}) =
   let
      val {get = conInfo: Con.t -> ConInfo.t,
           set = setConInfo, ...} =
         Property.getSetOnce
         (Con.plist,
          Property.initRaise ("RemoveUnused2.conInfo", Con.layout))
      fun newConInfo (con, args, dummy) =
         setConInfo (con, ConInfo.new {args = args, dummy = dummy})

      val {get = tyconInfo: Tycon.t -> TyconInfo.t,
           set = setTyconInfo, ...} =
         Property.getSetOnce
         (Tycon.plist,
          Property.initRaise ("RemoveUnused2.tyconInfo", Tycon.layout))
      fun newTyconInfo (tycon, cons, dummy) =
         setTyconInfo (tycon, TyconInfo.new {cons = cons, dummy = dummy})

      val {get = typeInfo: Type.t -> TypeInfo.t,
           destroy, ...} =
         Property.destGet
         (Type.plist,
          Property.initFun (fn _ => TypeInfo.new ()))

      val {get = varInfo: Var.t -> VarInfo.t,
           set = setVarInfo, ...} =
         Property.getSetOnce
         (Var.plist,
          Property.initRaise ("RemoveUnused2.varInfo", Var.layout))
      fun newVarInfo (var, ty) =
         setVarInfo (var, VarInfo.new ty)

      val {get = labelInfo: Label.t -> LabelInfo.t,
           set = setLabelInfo, ...} =
         Property.getSetOnce
         (Label.plist,
          Property.initRaise ("RemoveUnused2.labelInfo", Label.layout))

      val {get = funcInfo: Func.t -> FuncInfo.t,
           set = setFuncInfo, ...} =
         Property.getSetOnce
         (Func.plist,
          Property.initRaise ("RemoveUnused2.funcInfo", Func.layout))


      val usedCon = ConInfo.used o conInfo
      val useCon = Used.use o usedCon
      fun visitCon (con: Con.t) = useCon con
      val whenUsedCon = fn (con, th) => ConInfo.whenUsed (conInfo con, th)

      val usedTycon = TyconInfo.used o tyconInfo
      val useTycon = Used.use o usedTycon
      fun visitTycon (tycon: Tycon.t) = useTycon tycon
      val isUsedTycon = Used.isUsed o usedTycon

      fun visitType (ty: Type.t) =
         let
            val ti = typeInfo ty
            val used = TypeInfo.used' ti
         in
            if !used
               then ()
            else let
                    val  () = used := true
                    datatype z = datatype Type.dest
                    datatype z = datatype ObjectCon.t
                    val () =
                       case Type.dest ty of
                          Datatype tycon => visitTycon tycon
                        | Object {args, con} =>
                             let
                                val () = Prod.foreach (args, visitType)
                                val () =
                                   case con of
                                      Con con => visitCon con
                                    | Tuple => ()
                                    | Vector => ()
                             in
                                ()
                             end
                        | Weak ty => visitType ty
                        | _ => ()
                 in
                    ()
                 end
         end
      val visitTypeTh = fn ty => fn () => visitType ty

      val tyVar = VarInfo.ty o varInfo
      val usedVar = VarInfo.used o varInfo
      val useVar = Used.use o usedVar
      val isUsedVar = Used.isUsed o usedVar
      val whenUsedVar = fn (var, th) => VarInfo.whenUsed (varInfo var, th)
      fun flowVarInfoTyVarInfoTy ((vi, _), (vi', _)) =
         Used.<= (VarInfo.used vi, VarInfo.used vi')
      fun flowVarInfoTysVarInfoTys (xs, ys) =
         Vector.foreach2 (xs, ys, flowVarInfoTyVarInfoTy)
      fun flowVarInfoTyVar ((vi, _), x) =
         Used.<= (VarInfo.used vi, usedVar x)
      fun flowVarInfoTysVars (xs, ys) =
         Vector.foreach2 (xs, ys, flowVarInfoTyVar)

      val newVarInfo = fn (var, ty) =>
         (newVarInfo (var, ty)
          ; whenUsedVar (var, visitTypeTh ty))

      val visitLabelInfo = LabelInfo.use
      val visitLabelInfoTh = fn li => fn () => visitLabelInfo li
      val visitLabel = visitLabelInfo o labelInfo
      val visitLabelTh = fn l => fn () => visitLabel l
      val visitFuncInfo = FuncInfo.use
      val visitFunc = visitFuncInfo o funcInfo

      fun visitVar (x: Var.t) = useVar x
      fun visitVars (xs: Var.t Vector.t) = Vector.foreach (xs, visitVar)
      fun visitExp (e: Exp.t) =
         case e of
            Const _ => ()
          | Inject {sum, variant} =>
               (visitTycon sum
                ; visitVar variant)
          | Object {args, con} =>
               let
                  val () =
                     case con of
                        NONE => visitVars args
                      | SOME con =>
                           let
                              val ci = conInfo con
                              val () = ConInfo.con ci
                              val ciArgs =
                                 Vector.map
                                 (Prod.dest (ConInfo.args ci), #elt)
                              val () = flowVarInfoTysVars (ciArgs, args)
                           in
                              ()
                           end
               in
                  ()
               end
          | PrimApp {prim, args, ...} =>
               let
                  val () = visitVars args
                  datatype z = datatype Type.dest
                  datatype z = datatype ObjectCon.t
                  fun deconType (ty: Type.t) =
                     let
                        val ti = typeInfo ty
                        val deconed = TypeInfo.deconed' ti
                     in
                        if !deconed
                           then ()
                        else let
                                val () = deconed := true
                                val () =
                                   case Type.dest ty of
                                      Datatype t =>
                                         Vector.foreach
                                         (TyconInfo.cons (tyconInfo t),
                                          fn con => deconCon con)
                                    | Object {args, con} =>
                                         let
                                            fun default () =
                                               Vector.foreach
                                               (Prod.dest args, fn {elt, isMutable} =>
                                                if isMutable
                                                   then ()
                                                else deconType elt)
                                            val () =
                                               case con of
                                                  Con con => deconCon con
                                                | Tuple => default ()
                                                | Vector => default ()
                                         in
                                            ()
                                         end
                                    | _ => ()
                             in
                                ()
                             end
                     end
                  and deconCon con =
                     let
                        val ci = conInfo con
                        val () = ConInfo.decon ci
                        val () =
                           Vector.foreach
                           (Prod.dest (ConInfo.args ci), fn {elt = (x, t), isMutable} =>
                            (VarInfo.use x
                             ; if isMutable then () else deconType t))
                     in
                        ()
                     end
                  val () =
                     case Prim.name prim of
                        Prim.Name.MLton_eq =>
                           (* MLton_eq may be used on datatypes used as enums. *)
                           deconType (tyVar (Vector.first args))
                      | Prim.Name.MLton_equal =>
                           (* MLton_equal will be expanded by poly-equal into uses
                            * of constructors as patterns.
                            *)
                           deconType (tyVar (Vector.first args))
                      | Prim.Name.MLton_hash =>
                           (* MLton_hash will be expanded by poly-hash into uses
                            * of constructors as patterns.
                            *)
                           deconType (tyVar (Vector.first args))
(*
                      | Prim.Name.MLton_size =>
                           deconType (tyVar (Vector.first args))
*)
                      | _ => ()
               in
                  ()
               end
          | Select {base, offset} =>
               let
                  datatype z = datatype Base.t
                  datatype z = datatype ObjectCon.t
               in
                  case base of
                     Object base =>
                        let
                           val () = visitVar base
                           val () =
                              case Type.dest (tyVar base) of
                                 Type.Object {con, ...} =>
                                    (case con of
                                        Con con =>
                                           let
                                              val ci = conInfo con
                                              val ciArgs = ConInfo.args ci
                                              val {elt = (vi, _), ...} =
                                                 Prod.sub (ciArgs, offset)

                                              val () = ConInfo.decon ci
                                              val () = VarInfo.use vi
                                           in
                                              ()
                                           end
                                      | Tuple => ()
                                      | Vector => Error.bug "RemoveUnused2.visitExp: Select:non-Con|Tuple")
                               | _ => Error.bug "RemovUnused2.visitExp: Select:non-Object"
                        in
                           ()
                        end
                   | VectorSub {index, vector} =>
                        (visitVar index
                         ; visitVar vector)
               end
          | Var x => visitVar x
      val visitExpTh = fn e => fn () => visitExp e
      fun maybeVisitVarExp (var, exp) =
         Option.app (var, fn var =>
                     VarInfo.whenUsed (varInfo var, visitExpTh exp))
      fun visitStatement s =
         case s of
            Bind {exp, ty, var} =>
               (Option.app (var, fn var => newVarInfo (var, ty))
                ; if Exp.maySideEffect exp
                     then (visitType ty
                           ; visitExp exp)
                  else maybeVisitVarExp (var, exp))
          | Profile _ => ()
          | Update {base, offset, value} =>
               let
                  datatype z = datatype Base.t
                  datatype z = datatype ObjectCon.t
               in
                  case base of
                     Object base =>
                        (case Type.dest (tyVar base) of
                            Type.Object {con, ...} =>
                               (case con of
                                   Con con =>
                                      let
                                         val ci = conInfo con
                                         val ciArgs = ConInfo.args ci
                                         val {elt = (vi, _), ...} =
                                            Prod.sub (ciArgs, offset)
                                      in
                                         VarInfo.whenUsed
                                         (vi, fn () =>
                                          (ConInfo.decon ci
                                           ; visitVar base
                                           ; visitVar value))
                                      end
                                 | Tuple =>
                                      (visitVar base
                                       ; visitVar value)
                                 | Vector => Error.bug "RemoveUnused2.visitStatement: Update:non-Con|Tuple")
                          | _ => Error.bug "RemoveUnused2.visitStatement: Update:non-Object")
                   | VectorSub {index, vector} =>
                        (visitVar index
                         ; visitVar vector
                         ; visitVar value)
               end
      fun visitTransfer (t: Transfer.t, fi: FuncInfo.t) =
         case t of
            Arith {args, overflow, success, ty, ...} =>
               (visitVars args
                ; visitLabel overflow
                ; visitLabel success
                ; visitType ty)
          | Bug => ()
          | Call {args, func, return} =>
               let
                  datatype u = None
                             | Caller
                             | Some of Label.t
                  val (cont, handler) =
                     case return of
                        Return.Dead => (None, None)
                      | Return.NonTail {cont, handler} =>
                           (Some cont,
                            case handler of
                               Handler.Caller => Caller
                             | Handler.Dead => None
                             | Handler.Handle h => Some h)
                      | Return.Tail => (Caller, Caller)
                  val fi' = funcInfo func

                  val () = flowVarInfoTysVars (FuncInfo.args fi', args)
                  val () =
                     case cont of
                        None => ()
                      | Caller =>
                           let
                              val () =
                                 case (FuncInfo.returns fi,
                                       FuncInfo.returns fi') of
                                    (SOME xts, SOME xts') =>
                                       flowVarInfoTysVarInfoTys (xts, xts')
                                  | _ => ()
                              val () = FuncInfo.flowReturns (fi', fi)
                           in
                              ()
                           end
                      | Some l =>
                           let
                              val li = labelInfo l
                              val () =
                                 Option.app
                                 (FuncInfo.returns fi', fn xts =>
                                  flowVarInfoTysVarInfoTys
                                  (LabelInfo.args li, xts))
                              val () =
                                 FuncInfo.whenReturns
                                 (fi', visitLabelInfoTh li)
                           in
                              ()
                           end
                  val () =
                     case handler of
                        None => ()
                      | Caller =>
                           let
                              val () =
                                 case (FuncInfo.raises fi,
                                       FuncInfo.raises fi') of
                                    (SOME xts, SOME xts') =>
                                       flowVarInfoTysVarInfoTys (xts, xts')
                                  | _ => ()
                              val () = FuncInfo.flowRaises (fi', fi)
                           in
                              ()
                           end
                      | Some l =>
                           let
                              val li = labelInfo l
                              val () =
                                 Option.app
                                 (FuncInfo.raises fi', fn xts =>
                                  flowVarInfoTysVarInfoTys
                                  (LabelInfo.args li, xts))
                              val () =
                                 FuncInfo.whenRaises (fi', visitLabelInfoTh li)
                           in
                              ()
                           end
                  val () = visitFuncInfo fi'
               in
                  ()
               end
          | Case {test, cases, default} =>
               let
                  val () = visitVar test
               in
                  case cases of
                     Cases.Word (_, cs) =>
                        (Vector.foreach (cs, visitLabel o #2)
                         ; Option.app (default, visitLabel))
                   | Cases.Con cases =>
                        if Vector.isEmpty cases
                           then Option.app (default, visitLabel)
                        else let
                                val () =
                                   Vector.foreach
                                   (cases, fn (con, l) =>
                                    let
                                       val ci = conInfo con
                                       val () = ConInfo.decon ci
                                       val () =
                                          ConInfo.whenConed
                                          (ci, visitLabelTh l)
                                    in
                                       ()
                                    end)
                                val tycon =
                                   case Type.dest (tyVar test) of
                                      Type.Datatype tycon => tycon
                                    | _ => Error.bug "RemoveUnused2.visitTransfer: Case:non-Datatype"
                                val cons = TyconInfo.cons (tyconInfo tycon)
                             in
                                case default of
                                   NONE => ()
                                 | SOME l =>
                                      Vector.foreach
                                      (cons, fn con =>
                                       if Vector.exists
                                          (cases, fn (c, _) =>
                                           Con.equals(c, con))
                                          then ()
                                       else
                                          ConInfo.whenConed
                                          (conInfo con, visitLabelTh l))
                             end
               end
          | Goto {dst, args} =>
               let
                  val li = labelInfo dst
                  val () = flowVarInfoTysVars (LabelInfo.args li, args)
                  val () = visitLabelInfo li
               in
                  ()
               end
          | Raise xs =>
               (FuncInfo.raisee fi
                ; flowVarInfoTysVars (valOf (FuncInfo.raises fi), xs))
          | Return xs =>
               (FuncInfo.return fi
                ; flowVarInfoTysVars (valOf (FuncInfo.returns fi), xs))
          | Runtime {args, return, ...} =>
               (visitVars args
                ; visitLabel return)
      fun visitBlock (Block.T {statements, transfer, ...}, fi: FuncInfo.t) =
         (Vector.foreach (statements, visitStatement)
          ; visitTransfer (transfer, fi))
      val visitBlockTh = fn (b, fi) => fn () => visitBlock (b, fi)
      (* Visit all reachable expressions. *)
      val () =
         Vector.foreach
         (datatypes, fn Datatype.T {tycon, cons} =>
          let
             val dummyCon = Con.newString "dummy"
             val dummyArgs = Prod.empty ()
             val dummy = {con = dummyCon, args = dummyArgs}
             val () =
                newTyconInfo
                (tycon, Vector.map (cons, fn {con, ...} => con), dummy)
             val dummyTy = Type.conApp (dummyCon, dummyArgs)
             val dummyExp = Object {args = Vector.new0 (),
                                    con = SOME dummyCon}
             val dummy = {con = dummyCon, args = dummyArgs,
                          ty = dummyTy, exp = dummyExp}
             val () =
                Vector.foreach
                (cons, fn {con, args} =>
                 (newConInfo (con, args, dummy)
                  ; whenUsedCon (con, fn () => useTycon tycon)))
          in
             ()
          end)
      val () =
         let
            fun doitCon c =
               let
                  val ci = conInfo c
               in
                  ConInfo.use ci
                  ; ConInfo.con ci
                  ; ConInfo.decon ci
               end
         in
            useTycon Tycon.bool
            ; doitCon Con.truee
            ; doitCon Con.falsee
         end
      val () =
         Vector.foreach (globals, visitStatement)
      val () =
         List.foreach
         (functions, fn function =>
          let
             val {name, args, raises, returns, start, blocks, ...} =
                Function.dest function
             val () = Vector.foreach (args, newVarInfo)
             local
                fun doitVarTys vts =
                   Vector.map (vts, fn (x, t) => (varInfo x, t))
                fun doitTys ts =
                   Vector.map (ts, fn t => (VarInfo.new t, t))
                fun doitTys' ts =
                   Option.map (ts, doitTys)
             in
                val fi =
                   FuncInfo.new
                   {args = doitVarTys args,
                    raises = doitTys' raises,
                    returns = doitTys' returns}
             end
             val () = setFuncInfo (name, fi)
             val () = FuncInfo.whenUsed (fi, visitLabelTh start)
             val () =
                Vector.foreach
                (blocks, fn block as Block.T {label, args, ...} =>
                 let
                    val () = Vector.foreach (args, newVarInfo)
                    local
                       fun doitVarTys vts =
                          Vector.map (vts, fn (x, t) => (varInfo x, t))
                    in
                       val li =
                          LabelInfo.new
                          {args = doitVarTys args,
                           func = fi}
                    end
                    val () = setLabelInfo (label, li)
                    val () = LabelInfo.whenUsed (li, visitBlockTh (block, fi))
                 in
                    ()
                 end)
          in
             ()
          end)
      val () = visitFunc main

      (* Diagnostics *)
      val () =
         Control.diagnostics
         (fn display =>
          let open Layout
          in
             Vector.foreach
             (datatypes, fn Datatype.T {tycon, cons} =>
              display (seq [Tycon.layout tycon,
                            str ": ",
                            TyconInfo.layout (tyconInfo tycon),
                            str ": ",
                            Vector.layout
                            (fn {con, ...} =>
                             seq [Con.layout con,
                                  str " ",
                                  ConInfo.layout (conInfo con)])
                            cons]));
             display (str "\n");
             List.foreach
             (functions, fn f =>
              let
                 val {name, blocks, ...} = Function.dest f
              in
                 display (seq [Func.layout name,
                               str ": ",
                               FuncInfo.layout (funcInfo name)]);
                 Vector.foreach
                 (blocks, fn Block.T {label, ...} =>
                  display (seq [Label.layout label,
                                str ": ",
                                LabelInfo.layout (labelInfo label)]));
                 display (str "\n")
              end)
          end)

      (* Analysis is done,  Now build the resulting program. *)
      fun getWrapperLabel (l: Label.t,
                           args: (VarInfo.t * Type.t) vector) =
         let
            val li = labelInfo l
         in
            if Vector.forall2 (args, LabelInfo.args li, fn ((x, _), (y, _)) =>
                               VarInfo.isUsed x = VarInfo.isUsed y)
               then l
            else let
                    val tys =
                       Vector.keepAllMap (args, fn (x, ty) =>
                                          if VarInfo.isUsed x
                                             then SOME ty
                                          else NONE)
                 in
                    case List.peek
                         (LabelInfo.wrappers li, fn (args', _) =>
                          Vector.length args' = Vector.length tys
                          andalso
                          Vector.forall2 (args', tys, fn (ty', ty) =>
                                          Type.equals (ty', ty))) of
                         NONE =>
                            let
                                val liArgs = LabelInfo.args li
                                val l' = Label.newNoname ()
                                val (args', args'') =
                                   Vector.unzip
                                   (Vector.map2
                                    (args, liArgs, fn ((x, ty), (y, _)) =>
                                     let
                                        val z = Var.newNoname ()
                                     in
                                        (if VarInfo.isUsed x
                                            then SOME (z, ty) else NONE,
                                         if VarInfo.isUsed y
                                            then SOME z else NONE)
                                     end))
                                val args' =
                                   Vector.keepAllMap (args', fn x => x)
                                val (_, tys') = Vector.unzip args'
                                val args'' =
                                   Vector.keepAllMap (args'', fn x => x)
                                val block =
                                   Block.T {label = l',
                                            args =  args',
                                            statements = Vector.new0 (),
                                            transfer = Goto {dst = l,
                                                             args = args''}}
                                val () =
                                   List.push (LabelInfo.wrappers' li,
                                              (tys', l'))
                                val () =
                                   List.push (FuncInfo.wrappers' (LabelInfo.func li),
                                              block)
                            in
                               l'
                            end
                       | SOME (_, l') => l'
                 end
         end
      val getContWrapperLabel = getWrapperLabel
      val getHandlerWrapperLabel = getWrapperLabel
      fun getOriginalWrapperLabel l =
         getWrapperLabel
         (l, Vector.map (LabelInfo.args (labelInfo l), fn (_, t) =>
                         let
                            val x = VarInfo.new t
                            val () = VarInfo.use x
                         in
                            (x, t)
                         end))
      val getArithOverflowWrapperLabel = getOriginalWrapperLabel
      val getArithSuccessWrapperLabel = getOriginalWrapperLabel
      val getRuntimeWrapperLabel = getOriginalWrapperLabel
      fun getBugFunc (fi: FuncInfo.t): Label.t =
         (* Can't share the Bug block across different places because the
          * profile sourceInfo stack might be different.
          *)
         let
            val l = Label.newNoname ()
            val block = Block.T {label = l,
                                 args = Vector.new0 (),
                                 statements = Vector.new0 (),
                                 transfer = Bug}
            val () = List.push (FuncInfo.wrappers' fi, block)
         in
            l
         end
      fun getReturnFunc (fi: FuncInfo.t): Label.t =
         let
            val r = FuncInfo.returnLabel fi
         in
            case !r of
               NONE =>
                  let
                     val l = Label.newNoname ()
                     val returns = valOf (FuncInfo.returns fi)
                     val args =
                        Vector.keepAllMap
                        (returns, fn (vi, ty) =>
                         if VarInfo.isUsed vi
                            then SOME (Var.newNoname (), ty)
                         else NONE)
                     val xs = Vector.map (args, #1)
                     val block = Block.T {label = l,
                                          args = args,
                                          statements = Vector.new0 (),
                                          transfer = Return xs}
                     val () = r := SOME l
                     val () = List.push (FuncInfo.wrappers' fi, block)
                     val () = setLabelInfo (l, LabelInfo.new {func = fi,
                                                              args = returns})
                  in
                     l
                  end
             | SOME l => l
         end
      fun getReturnContFunc (fi, args) =
         getWrapperLabel (getReturnFunc fi, args)
      fun getRaiseFunc (fi: FuncInfo.t): Label.t =
         let
            val r = FuncInfo.raiseLabel fi
         in
            case !r of
               NONE =>
                  let
                     val l = Label.newNoname ()
                     val raises = valOf (FuncInfo.raises fi)
                     val args =
                        Vector.keepAllMap
                        (raises, fn (vi, ty) =>
                         if VarInfo.isUsed vi
                            then SOME (Var.newNoname (), ty)
                         else NONE)
                     val xs = Vector.map (args, #1)
                     val block = Block.T {label = l,
                                          args = args,
                                          statements = Vector.new0 (),
                                          transfer = Raise xs}
                     val () = r := SOME l
                     val () = List.push (FuncInfo.wrappers' fi, block)
                     val () = setLabelInfo (l, LabelInfo.new {func = fi,
                                                              args = raises})
                  in
                     l
                  end
             | SOME l => l
         end
      fun getRaiseHandlerFunc (fi, args) =
         getWrapperLabel (getRaiseFunc fi, args)

      fun simplifyType (ty: Type.t): Type.t =
         let
            val ti = typeInfo ty
            val simplify = TypeInfo.simplify' ti
         in
            case !simplify of
               NONE => let
                          datatype z = datatype Type.dest
                          datatype z = datatype ObjectCon.t
                          val ty =
                             case Type.dest ty of
                                Object {args, con} =>
                                   (case con of
                                       Con con =>
                                          let
                                             val ci = conInfo con
                                          in
                                             case (ConInfo.isConed ci,
                                                   ConInfo.isDeconed ci) of
                                                (false, _) =>
                                                   #ty (ConInfo.dummy ci)
                                              | (true, true) =>
                                                   Type.object
                                                   {args = Prod.keepAllMap
                                                           (ConInfo.args ci, fn (x,t) =>
                                                            if VarInfo.isUsed x
                                                               then SOME (simplifyType t)
                                                            else NONE),
                                                    con = Con con}
                                              | (true, false) =>
                                                   #ty (ConInfo.dummy ci)
                                          end
                                     | _ =>
                                          Type.object
                                          {args = Prod.map (args, simplifyType),
                                           con = con})
                              | Weak ty => Type.weak (simplifyType ty)
                              | _ => ty
                       in
                          simplify := SOME ty
                          ; ty
                       end
             | SOME ty => ty
         end

      val datatypes =
         Vector.keepAllMap
         (datatypes, fn Datatype.T {tycon, cons} =>
          if isUsedTycon tycon
             then let
                     val needsDummy : bool ref = ref false
                     val cons =
                        Vector.keepAllMap
                        (cons, fn {con, ...} =>
                         let
                            val ci = conInfo con
                            fun addDummy () =
                               if !needsDummy
                                  then NONE
                               else let
                                       val () = needsDummy := true
                                    in
                                       SOME (TyconInfo.dummy (tyconInfo tycon))
                                    end
                         in
                            case (ConInfo.isConed ci,
                                  ConInfo.isDeconed ci) of
                               (false, _) =>
                                  if ConInfo.isUsed ci
                                     then addDummy ()
                                  else NONE
                             | (true, true) =>
                                  SOME {con = con,
                                        args = Prod.keepAllMap
                                        (ConInfo.args ci, fn (x, ty) =>
                                         if VarInfo.isUsed x
                                            then SOME (simplifyType ty)
                                         else NONE)}
                             | (true, false) =>
                                  addDummy ()
                         end)
                     val num = Vector.length cons
                     val () = TyconInfo.numCons' (tyconInfo tycon) := num
                  in
                     SOME (Datatype.T {tycon = tycon, cons = cons})
                  end
          else NONE)

      fun simplifyExp (e: Exp.t): Exp.t =
         case e of
            Object {con, args} =>
               (case con of
                   NONE => e
                 | SOME con =>
                      let
                         val ci = conInfo con
                      in
                         if ConInfo.isDeconed ci
                            then let
                                    val ciArgs =
                                       Vector.map
                                       (Prod.dest (ConInfo.args ci), #elt)
                                 in
                                    Object {con = SOME con,
                                            args = (Vector.keepAllMap2
                                                    (args, ciArgs,
                                                     fn (x, (y, _)) =>
                                                     if VarInfo.isUsed y
                                                        then SOME x
                                                     else NONE))}
                                 end
                         else #exp (ConInfo.dummy ci)
                      end)
          | Select {base, offset} =>
               let
                  datatype z = datatype Base.t
               in
                  case base of
                     Object base =>
                        let
                           datatype z = datatype ObjectCon.t
                           datatype z = datatype Type.dest
                        in
                           case Type.dest (tyVar base) of
                              Object {con, ...} =>
                                 (case con of
                                     Con con =>
                                        let
                                           val ci = conInfo con
                                           val ciArgs = ConInfo.args ci
                                           val offset =
                                              Int.fold
                                              (0, offset, 0, fn (i, offset) =>
                                               if (VarInfo.isUsed o #1 o #elt)
                                                  (Prod.sub (ciArgs, i))
                                                  then offset + 1
                                               else offset)
                                        in
                                           Select {base = Base.Object base,
                                                   offset = offset}
                                        end
                                   | Tuple => e
                                   | Vector => Error.bug "RemoveUnused2.simplifyExp: Update:non-Con|Tuple")
                            | _ => Error.bug "RemoveUnused2.simplifyExp:Select:non-Object"
                        end
                   | _ => e
               end
          | _ => e
      fun simplifyStatement (s : Statement.t) : Statement.t option =
         case s of
            Bind {exp, ty, var} =>
               let
                  fun doit' var =
                     SOME (Statement.Bind
                           {var = var,
                            ty = simplifyType ty,
                            exp = simplifyExp exp})
                  fun doit var' =
                     if Exp.maySideEffect exp
                        then doit' var
                     else if isSome var'
                             then doit' var'
                          else NONE
               in
                  case var of
                     SOME var => if isUsedVar var
                                    then doit (SOME var)
                                 else doit NONE
                   | NONE => doit NONE
               end
          | Profile _ => SOME s
          | Update {base, offset, value} =>
               let
                  datatype z = datatype Base.t
               in
                  case base of
                     Object base =>
                        let
                           datatype z = datatype ObjectCon.t
                           datatype z = datatype Type.dest
                        in
                           case Type.dest (tyVar base) of
                              Object {con, ...} =>
                                 (case con of
                                     Con con =>
                                        let
                                           val ci = conInfo con
                                           val ciArgs = ConInfo.args ci
                                           fun argIsUsed i =
                                              VarInfo.isUsed
                                              (#1 (#elt (Prod.sub (ciArgs, i))))
                                        in
                                           if argIsUsed offset
                                              then
                                                 let
                                                    val offset =
                                                       Int.fold
                                                       (0, offset, 0,
                                                        fn (i, offset) =>
                                                        if argIsUsed i
                                                           then offset + 1
                                                        else offset)
                                                 in
                                                    SOME
                                                    (Update
                                                     {base = Base.Object base,
                                                      offset = offset,
                                                      value = value})
                                                 end
                                           else NONE
                                        end
                                   | Tuple => SOME s
                                   | Vector => Error.bug "RemoveUnused2.simplifyStatement: Update:non-Con|Tuple")
                            | _ => Error.bug "RemoveUnused2.simplifyStatement: Select:non-Object"
                        end
                   | _ => SOME s
               end
      fun simplifyStatements (ss: Statement.t Vector.t) : Statement.t Vector.t =
         Vector.keepAllMap (ss, simplifyStatement)
      fun simplifyTransfer (t: Transfer.t, fi: FuncInfo.t): Transfer.t =
         case t of
            Arith {prim, args, overflow, success, ty} =>
               Arith {prim = prim,
                      args = args,
                      overflow = getArithOverflowWrapperLabel overflow,
                      success = getArithSuccessWrapperLabel success,
                      ty = simplifyType ty}
          | Bug => Bug
          | Call {func, args, return} =>
               let
                  val fi' = funcInfo func
                  datatype u = None
                             | Caller
                             | Some of Label.t
                  val (cont, handler) =
                     case return of
                        Return.Dead => (None, None)
                      | Return.NonTail {cont, handler} =>
                           (Some cont,
                            case handler of
                               Handler.Caller => Caller
                             | Handler.Dead => None
                             | Handler.Handle h => Some h)
                      | Return.Tail => (Caller, Caller)
                  val cont =
                     if FuncInfo.mayReturn fi'
                        then case cont of
                                None =>
                                   Error.bug "RemoveUnused2.simplifyTransfer: cont:None"
                              | Caller =>
                                   (if (case (FuncInfo.returns fi,
                                              FuncInfo.returns fi') of
                                           (SOME xts, SOME yts) =>
                                              Vector.forall2
                                              (xts, yts, fn ((x, _), (y, _)) =>
                                               VarInfo.isUsed x = VarInfo.isUsed y)
                                         | _ => Error.bug "RemoveUnused2.simplifyTransfer: cont:Caller")
                                       then Caller
                                    else Some (getReturnContFunc
                                               (fi, valOf (FuncInfo.returns fi'))))
                              | Some l =>
                                   Some (getContWrapperLabel
                                         (l, valOf (FuncInfo.returns fi')))
                     else None
                  val handler =
                     if FuncInfo.mayRaise fi'
                        then (case handler of
                                 None =>
                                    Error.bug "RemoveUnused2.simplifyTransfer: handler:None"
                               | Caller =>
                                    (if (case (FuncInfo.raises fi,
                                               FuncInfo.raises fi') of
                                            (SOME xts, SOME yts) =>
                                               Vector.forall2
                                               (xts, yts, fn ((x, _), (y, _)) =>
                                                VarInfo.isUsed x = VarInfo.isUsed y)
                                          | _ => Error.bug "RemoveUnused2.simplifyTransfer: handler:Caller")
                                        then Caller
                                     else Some (getRaiseHandlerFunc
                                                (fi, valOf (FuncInfo.raises fi'))))
                               | Some l =>
                                    Some (getHandlerWrapperLabel
                                          (l, valOf (FuncInfo.raises fi'))))
                        else None
                  val return =
                     case (cont, handler) of
                        (None, None) => Return.Dead
                      | (None, Caller) => Return.Tail
                      | (None, Some h) =>
                           Return.NonTail
                           {cont = getBugFunc fi,
                            handler = Handler.Handle h}
                      | (Caller, None) => Return.Tail
                      | (Caller, Caller) => Return.Tail
                      | (Caller, Some h) =>
                           Return.NonTail
                           {cont = getReturnContFunc
                            (fi, valOf (FuncInfo.returns fi')),
                            handler = Handler.Handle h}
                      | (Some c, None) =>
                           Return.NonTail
                           {cont = c,
                            handler = Handler.Dead}
                      | (Some c, Caller) =>
                           Return.NonTail
                           {cont = c,
                            handler = Handler.Caller}
                      | (Some c, Some h) =>
                           Return.NonTail
                           {cont = c,
                            handler = Handler.Handle h}

                  val args =
                     Vector.keepAllMap2
                     (args, FuncInfo.args fi', fn (x, (y, _)) =>
                      if VarInfo.isUsed y
                         then SOME x
                      else NONE)
               in
                  Call {func = func,
                        args = args,
                        return = return}
               end
          | Case {test, cases = Cases.Con cases, default} =>
               let
                  val cases =
                     Vector.keepAllMap
                     (cases, fn (con, l) =>
                      let
                         val ci = conInfo con
                      in
                         if ConInfo.isConed ci
                            then SOME (con, l)
                         else NONE
                      end)
                  fun keep default = Case {test = test,
                                           cases = Cases.Con cases,
                                           default = default}
                  fun none () = keep NONE
               in
                  case default of
                     NONE => none ()
                   | SOME l => if Vector.isEmpty cases
                                  then if LabelInfo.isUsed (labelInfo l)
                                          then Goto {dst = l, args = Vector.new0 ()}
                                       else Bug
                               else let
                                       val tycon =
                                          case Type.dest (tyVar test) of
                                             Type.Datatype tycon => tycon
                                           | _ => Error.bug "RemoveUnused2.simplifyTransfer: Case:non-Datatype"
                                       val numCons = TyconInfo.numCons (tyconInfo tycon)
                                    in
                                       if Vector.length cases = numCons
                                          then none ()
                                       else keep (SOME l)
                                    end
               end
          | Case {test, cases, default} =>
               Case {test = test,
                     cases = cases,
                     default = default}
          | Goto {dst, args} =>
               Goto {dst = dst,
                     args = (Vector.keepAllMap2
                             (args, LabelInfo.args (labelInfo dst),
                              fn (x, (y, _)) => if VarInfo.isUsed y
                                                   then SOME x
                                                else NONE))}
          | Raise xs =>
               Raise (Vector.keepAllMap2
                      (xs, valOf (FuncInfo.raises fi),
                       fn (x, (y, _)) => if VarInfo.isUsed y
                                            then SOME x
                                         else NONE))
          | Return xs =>
               Return (Vector.keepAllMap2
                       (xs, valOf (FuncInfo.returns fi),
                        fn (x, (y, _)) => if VarInfo.isUsed y
                                             then SOME x
                                          else NONE))
          | Runtime {prim, args, return} =>
               Runtime {prim = prim,
                        args = args,
                        return = getRuntimeWrapperLabel return}
      val simplifyTransfer =
         Trace.trace
         ("RemoveUnused2.simplifyTransfer",
          Layout.tuple2 (Transfer.layout, FuncInfo.layout), Transfer.layout)
         simplifyTransfer
      fun simplifyBlock (Block.T {label, args, statements, transfer}): Block.t option =
         let
            val li = labelInfo label
         in
            if LabelInfo.isUsed li
               then let
                       val args =
                          Vector.keepAllMap2
                          (LabelInfo.args li, args, fn ((vi, _), (x, ty)) =>
                           if VarInfo.isUsed vi
                              then SOME (x, simplifyType ty)
                           else NONE)
                       val statements = simplifyStatements statements
                       val transfer =
                          simplifyTransfer (transfer, LabelInfo.func li)
                    in
                       SOME (Block.T {label = label,
                                      args = args,
                                      statements = statements,
                                      transfer = transfer})
                    end
            else NONE
         end
      fun simplifyBlocks (bs: Block.t Vector.t): Block.t Vector.t =
         Vector.keepAllMap (bs, simplifyBlock)
      val globals = simplifyStatements globals
      val shrink = shrinkFunction {globals = globals}
      fun simplifyFunction (f: Function.t): Function.t option =
         let
            val {args, blocks, mayInline, name, start, ...} = Function.dest f
            val fi = funcInfo name
         in
            if FuncInfo.isUsed fi
               then let
                       val args =
                          Vector.keepAllMap2
                          (FuncInfo.args fi, args, fn ((vi, _), (x, ty)) =>
                           if VarInfo.isUsed vi
                              then SOME (x, simplifyType ty)
                           else NONE)
                       val blocks = simplifyBlocks blocks
                       val wrappers = Vector.fromList (FuncInfo.wrappers fi)
                       val blocks = Vector.concat [wrappers, blocks]
                       val returns =
                          case FuncInfo.returns fi of
                             NONE => NONE
                           | SOME xts =>
                                if FuncInfo.mayReturn fi
                                   then SOME (Vector.keepAllMap
                                              (xts, fn (x, ty) =>
                                               if VarInfo.isUsed x
                                                  then SOME (simplifyType ty)
                                               else NONE))
                                else NONE
                       val raises =
                          case FuncInfo.raises fi of
                             NONE => NONE
                           | SOME xts =>
                                if FuncInfo.mayRaise fi
                                   then SOME (Vector.keepAllMap
                                              (xts, fn (x, ty) =>
                                               if VarInfo.isUsed x
                                                  then SOME (simplifyType ty)
                                               else NONE))
                                else NONE
                    in
                       SOME (shrink (Function.new {args = args,
                                                   blocks = blocks,
                                                   mayInline = mayInline,
                                                   name = name,
                                                   raises = raises,
                                                   returns = returns,
                                                   start = start}))
                    end
            else NONE
         end
      fun simplifyFunctions (fs: Function.t List.t): Function.t List.t =
         List.keepAllMap (fs, simplifyFunction)
      val functions = simplifyFunctions functions
      val program = Program.T {datatypes = datatypes,
                               globals = globals,
                               functions = functions,
                               main = main}
      val () = destroy ()
      val () = Program.clearTop program
   in
      program
   end

end
