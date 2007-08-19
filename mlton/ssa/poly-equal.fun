(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PolyEqual (S: POLY_EQUAL_STRUCTS): POLY_EQUAL = 
struct

open S

type int = Int.t

(*
 * This pass implements polymorphic equality.
 *
 * For each datatype tycon and vector type, it builds an equality function and
 * translates calls to = into calls to that function.
 *
 * Also generates calls to primitives intInfEqual and wordEqual.
 *
 * For tuples, it does the equality test inline.  I.E. it does not create
 * a separate equality function for each tuple type.
 *
 * All equality functions are only created if necessary, i.e. if equality
 * is actually used at a type.
 *
 * Optimizations:
 *  - For datatype tycons that are enumerations, do not build a case dispatch,
 *    just use eq, since you know the backend will represent these as ints.
 *  - Deep equality always does an eq test first.
 *  - If one argument to = is a constant int and the type will get translated
 *    to an IntOrPointer, then just use eq instead of the full equality.  This
 *    is important for implementing code like the following efficiently:
 *       if x = 0  ...    (where x is an IntInf.int)
 *)

open Exp Transfer

structure Dexp =
   struct
      open DirectExp

      fun add (e1: t, e2: t, s): t =
         primApp {prim = Prim.wordAdd s,
                  targs = Vector.new0 (),
                  args = Vector.new2 (e1, e2),
                  ty = Type.word s}

      fun conjoin (e1: t, e2: t): t =
         casee {test = e1,
                cases = Con (Vector.new2 ({con = Con.truee,
                                           args = Vector.new0 (),
                                           body = e2},
                                          {con = Con.falsee,
                                           args = Vector.new0 (),
                                           body = falsee})),
                default = NONE,
                ty = Type.bool}

      fun disjoin (e1: t, e2:t ): t =
         casee {test = e1,
                cases = Con (Vector.new2 ({con = Con.truee,
                                           args = Vector.new0 (),
                                           body = truee},
                                          {con = Con.falsee,
                                           args = Vector.new0 (),
                                           body = e2})),
                default = NONE,
                ty = Type.bool}
   end

fun polyEqual (Program.T {datatypes, globals, functions, main}) =
   let
      val shrink = shrinkFunction {globals = globals}
      val {get = tyconInfo: Tycon.t -> {isEnum: bool,
                                        cons: {con: Con.t,
                                               args: Type.t vector} vector},
           set = setTyconInfo, ...} =
         Property.getSetOnce
         (Tycon.plist, Property.initRaise ("PolyEqual.info", Tycon.layout))
      val isEnum = #isEnum o tyconInfo
      val tyconCons = #cons o tyconInfo
      val {get = varInfo: Var.t -> {isConst: bool},
           set = setVarInfo, ...} =
         Property.getSetOnce (Var.plist, Property.initConst {isConst = false})
      val _ =
         Vector.foreach
         (datatypes, fn Datatype.T {tycon, cons} =>
          setTyconInfo (tycon,
                        {isEnum = Vector.forall (cons, fn {args, ...} =>
                                                 Vector.isEmpty args),
                         cons = cons}))
      val newFunctions: Function.t list ref = ref []
      val {get = getEqualFunc: Tycon.t -> Func.t option, 
           set = setEqualFunc, ...} =
         Property.getSet (Tycon.plist, Property.initConst NONE)
      val {get = getVectorEqualFunc: Type.t -> Func.t option, 
           set = setVectorEqualFunc,
           destroy = destroyType} =
         Property.destGetSet (Type.plist, Property.initConst NONE)
      val returns = SOME (Vector.new1 Type.bool)
      val seqIndexWordSize = WordSize.seqIndex ()
      fun newFunction z =
         List.push (newFunctions,
                    Function.profile (shrink (Function.new z),
                                      SourceInfo.polyEqual))
      fun equalFunc (tycon: Tycon.t): Func.t =
         case getEqualFunc tycon of
            SOME f => f
          | NONE =>
               let
                  val name =
                     Func.newString (concat ["equal_", Tycon.originalName tycon])
                  val _ = setEqualFunc (tycon, SOME name)
                  val ty = Type.con (tycon, Vector.new0 ())
                  val arg1 = (Var.newNoname (), ty)
                  val arg2 = (Var.newNoname (), ty)
                  val args = Vector.new2 (arg1, arg2)
                  val darg1 = Dexp.var arg1
                  val darg2 = Dexp.var arg2
                  val cons = tyconCons tycon
                  val body =
                     Dexp.disjoin
                     (Dexp.eq (Dexp.var arg1, Dexp.var arg2, ty),
                      Dexp.casee
                      {test = darg1,
                       ty = Type.bool,
                       default = (if Vector.exists (cons, fn {args, ...} =>
                                                    0 = Vector.length args)
                                     then SOME Dexp.falsee
                                  else NONE),
                       cases =
                       Dexp.Con
                       (Vector.keepAllMap
                        (cons, fn {con, args} =>
                         if 0 = Vector.length args
                            then NONE
                         else
                            let
                               fun makeArgs () =
                                  Vector.map (args, fn ty =>
                                              (Var.newNoname (), ty))
                               val xs = makeArgs ()
                               val ys = makeArgs ()
                            in
                               SOME
                               {con = con,
                                args = xs,
                                body = 
                                Dexp.casee
                                {test = darg2,
                                 ty = Type.bool,
                                 default = if 1 = Vector.length cons
                                              then NONE
                                           else SOME Dexp.falsee,
                                              cases =
                                              Dexp.Con
                                              (Vector.new1
                                               {con = con,
                                                args = ys,
                                                body =
                                                Vector.fold2
                                                (xs, ys, Dexp.truee,
                                                 fn ((x, ty), (y, _), de) =>
                                                 Dexp.conjoin (de, equal (x, y, ty)))})}}
                            end))})
                  val (start, blocks) = Dexp.linearize (body, Handler.Caller)
                  val blocks = Vector.fromList blocks
                  val _ =
                     newFunction {args = args,
                                  blocks = blocks,
                                  mayInline = true,
                                  name = name,
                                  raises = NONE,
                                  returns = returns,
                                  start = start}
               in
                  name
               end
      and vectorEqualFunc (ty: Type.t): Func.t =
         case getVectorEqualFunc ty of
            SOME f => f
          | NONE =>
               let
                  (* Build two functions, one that checks the lengths and the
                   * other that loops.
                   *)
                  val name = Func.newString "vectorEqual"
                  val _ = setVectorEqualFunc (ty, SOME name)
                  val loop = Func.newString "vectorEqualLoop"
                  val vty = Type.vector ty
                  local
                     val v1 = (Var.newNoname (), vty)
                     val v2 = (Var.newNoname (), vty)
                     val args = Vector.new2 (v1, v2)
                     val dv1 = Dexp.var v1
                     val dv2 = Dexp.var v2
                     val body =
                        let
                          fun length x =
                             Dexp.primApp {prim = Prim.vectorLength,
                                           targs = Vector.new1 ty,
                                           args = Vector.new1 x,
                                           ty = Type.word seqIndexWordSize}
                        in
                           Dexp.disjoin
                           (Dexp.eq (Dexp.var v1, Dexp.var v2, vty),
                            Dexp.conjoin
                            (Dexp.eq (length dv1, length dv2, 
                                      Type.word seqIndexWordSize),
                             Dexp.call
                             {func = loop,
                              args = (Vector.new4 
                                      (Dexp.word (WordX.zero seqIndexWordSize),
                                       length dv1, dv1, dv2)),
                              ty = Type.bool}))
                        end
                     val (start, blocks) = Dexp.linearize (body, Handler.Caller)
                     val blocks = Vector.fromList blocks
                  in
                     val _ =
                        newFunction {args = args,
                                     blocks = blocks,
                                     mayInline = true,
                                     name = name,
                                     raises = NONE,
                                     returns = returns,
                                     start = start}
                  end
                  local
                     val i = (Var.newNoname (), Type.word seqIndexWordSize)
                     val len = (Var.newNoname (), Type.word seqIndexWordSize)
                     val v1 = (Var.newNoname (), vty)
                     val v2 = (Var.newNoname (), vty)
                     val args = Vector.new4 (i, len, v1, v2)
                     val di = Dexp.var i
                     val dlen = Dexp.var len
                     val dv1 = Dexp.var v1
                     val dv2 = Dexp.var v2
                     val body =
                        let
                           fun sub (v, i) =
                              Dexp.primApp {prim = Prim.vectorSub,
                                            targs = Vector.new1 ty,
                                            args = Vector.new2 (v, i),
                                            ty = ty}
                           val args =
                              Vector.new4 
                              (Dexp.add
                               (di, Dexp.word (WordX.one seqIndexWordSize), 
                                seqIndexWordSize),
                               dlen, dv1, dv2)
                        in
                           Dexp.disjoin 
                           (Dexp.eq (di, dlen, Type.word seqIndexWordSize),
                            Dexp.conjoin
                            (equalExp (sub (dv1, di), sub (dv2, di), ty),
                             Dexp.call {args = args,
                                        func = loop,
                                        ty = Type.bool}))
                        end
                     val (start, blocks) = Dexp.linearize (body, Handler.Caller)
                     val blocks = Vector.fromList blocks
                  in
                     val _ =
                        newFunction {args = args,
                                     blocks = blocks,
                                     mayInline = true,
                                     name = loop,
                                     raises = NONE,
                                     returns = returns,
                                     start = start}
                  end
               in
                  name
               end
      and equalExp (e1: Dexp.t, e2: Dexp.t, ty: Type.t): Dexp.t =
         Dexp.name (e1, fn x1 => Dexp.name (e2, fn x2 => equal (x1, x2, ty)))
      and equal (x1: Var.t, x2: Var.t, ty: Type.t): Dexp.t =
         let
            val dx1 = Dexp.var (x1, ty)
            val dx2 = Dexp.var (x2, ty)
            fun prim (p, targs) =
               Dexp.primApp {prim = p,
                             targs = targs, 
                             args = Vector.new2 (dx1, dx2),
                             ty = Type.bool}
            fun eq () = prim (Prim.eq, Vector.new1 ty)
            fun hasConstArg () = #isConst (varInfo x1) orelse #isConst (varInfo x2)
         in
            case Type.dest ty of
               Type.Array _ => eq ()
             | Type.CPointer => prim (Prim.cpointerEqual, Vector.new0 ())
             | Type.Datatype tycon =>
                  if isEnum tycon orelse hasConstArg ()
                     then eq ()
                  else Dexp.call {func = equalFunc tycon,
                                  args = Vector.new2 (dx1, dx2),
                                  ty = Type.bool}
             | Type.IntInf => if hasConstArg ()
                                 then eq ()
                              else prim (Prim.intInfEqual, Vector.new0 ())
             | Type.Ref _ => eq ()
             | Type.Tuple tys =>
                  let
                     val max = Vector.length tys - 1
                     (* test components i, i+1, ... *)
                     fun loop (i: int): Dexp.t =
                        if i > max
                           then Dexp.truee
                        else let
                                val ty = Vector.sub (tys, i)
                                fun select tuple =
                                   Dexp.select {tuple = tuple,
                                                offset = i,
                                                ty = ty}
                             in
                                Dexp.conjoin
                                (equalExp (select dx1, select dx2, ty),
                                 loop (i + 1))
                             end
                  in
                     loop 0
                  end
             | Type.Vector ty =>
                  Dexp.call {func = vectorEqualFunc ty,
                             args = Vector.new2 (dx1, dx2),
                             ty = Type.bool}
             | Type.Word s => prim (Prim.wordEqual s, Vector.new0 ())
             | _ => Error.bug "PolyEqual.equal: strange type"
         end
      fun loopBind (Statement.T {exp, var, ...}) =
         let
            fun const () = setVarInfo (valOf var, {isConst = true})
         in
            case exp of
               Const c =>
                  (case c of
                      Const.IntInf i =>
                         if Const.SmallIntInf.isSmall i
                            then const ()
                         else ()
                    | Const.Word _ => const ()
                    | _ => ())
             | ConApp {args, ...} =>
                  if Vector.isEmpty args then const () else ()
             | _ => ()
         end
      val _ = Vector.foreach (globals, loopBind)
      fun doit blocks =
         let
            val _ =
               Vector.foreach
               (blocks, fn Block.T {statements, ...} =>
                Vector.foreach (statements, loopBind))
            val blocks = 
               Vector.fold
               (blocks, [], 
                fn (Block.T {label, args, statements, transfer}, blocks) =>
                let
                   fun finish ({label, args, statements}, transfer) =
                      Block.T {label = label,
                               args = args,
                               statements = Vector.fromListRev statements,
                               transfer = transfer}
                   val (blocks, las) =
                      Vector.fold
                      (statements, 
                       (blocks, {label = label, args = args, statements = []}),
                       fn (stmt as Statement.T {exp, var, ...}, 
                           (blocks, las as {label, args, statements})) =>
                       let
                         fun normal () = (blocks,
                                          {label = label,
                                           args = args,
                                           statements = stmt::statements})
                       in
                         case exp of
                            PrimApp {prim, targs, args, ...} =>
                               (case (Prim.name prim, Vector.length targs) of
                                   (Prim.Name.MLton_equal, 1) =>
                                      let
                                         val ty = Vector.sub (targs, 0)
                                         fun arg i = Vector.sub (args, i)
                                         val l = Label.newNoname ()
                                         val (start',bs') =
                                            Dexp.linearizeGoto
                                            (equal (arg 0, arg 1, ty),
                                             Handler.Dead,
                                             l)
                                      in
                                        (finish (las, 
                                                 Goto {dst = start',
                                                       args = Vector.new0 ()})
                                         :: (bs' @ blocks),
                                         {label = l,
                                          args = Vector.new1 (valOf var, Type.bool),
                                          statements = []})
                                      end
                                 | _ => normal ())
                          | _ => normal ()
                       end)
                in
                   finish (las, transfer)
                   :: blocks
                end)
         in
            Vector.fromList blocks
         end
      val functions =
         List.revMap 
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
          in
             shrink (Function.new {args = args,
                                   blocks = doit blocks,
                                   mayInline = mayInline,
                                   name = name,
                                   raises = raises,
                                   returns = returns,
                                   start = start})
          end)
      val program =
         Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = (!newFunctions) @ functions,
                    main = main}
      val _ = destroyType ()
      val _ = Program.clearTop program
   in
      program
   end

end
