(* Copyright (C) 2009,2014 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PolyEqual (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S

(*
 * This pass implements polymorphic equality.
 *
 * For each datatype tycon and vector type, it builds an equality function and
 * translates calls to MLton_equal into calls to that function.
 *
 * Also generates calls to primitive wordEqual.
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
 *  - If one argument to = is a constant and the type will get translated to
 *    an IntOrPointer, then just use eq instead of the full equality.  This is
 *    important for implementing code like the following efficiently:
 *       if x = 0  ...    (where x is an IntInf.int)
 *
 * Also convert pointer equality on scalar types to type specific primitives.
 *)

open Exp Transfer

structure Dexp =
   struct
      open DirectExp

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

      fun disjoin (e1: t, e2:t): t =
         casee {test = e1,
                cases = Con (Vector.new2 ({con = Con.truee,
                                           args = Vector.new0 (),
                                           body = truee},
                                          {con = Con.falsee,
                                           args = Vector.new0 (),
                                           body = e2})),
                default = NONE,
                ty = Type.bool}

      local
         fun mk prim =
            fn (e1: t, e2: t, s) =>
            primApp {prim = prim s,
                     targs = Vector.new0 (),
                     args = Vector.new2 (e1, e2),
                     ty = Type.word s}
      in
         val add = mk Prim.wordAdd
         val andb = mk Prim.wordAndb
         val orb = mk Prim.wordOrb
      end

      fun wordEqual (e1: t, e2: t, s): t =
         primApp {prim = Prim.wordEqual s,
                  targs = Vector.new0 (),
                  args = Vector.new2 (e1, e2),
                  ty = Type.bool}
   end

fun transform (Program.T {datatypes, globals, functions, main}) =
   let
      val {get = funcInfo: Func.t -> {hasEqual: bool},
           set = setFuncInfo, ...} =
         Property.getSet (Func.plist, Property.initConst {hasEqual = false})
      val {get = labelInfo: Label.t -> {hasEqual: bool},
           set = setLabelInfo, ...} =
         Property.getSet (Label.plist, Property.initConst {hasEqual = false})
      val {get = varInfo: Var.t -> {isConst: bool},
           set = setVarInfo, ...} =
         Property.getSetOnce (Var.plist, Property.initConst {isConst = false})
      val {get = tyconInfo: Tycon.t -> {isEnum: bool,
                                        cons: {con: Con.t,
                                               args: Type.t vector} vector},
           set = setTyconInfo, ...} =
         Property.getSetOnce
         (Tycon.plist, Property.initRaise ("PolyEqual.tyconInfo", Tycon.layout))
      val isEnum = #isEnum o tyconInfo
      val tyconCons = #cons o tyconInfo
      val {get = getTyconEqualFunc: Tycon.t -> Func.t option,
           set = setTyconEqualFunc, ...} =
         Property.getSet (Tycon.plist, Property.initConst NONE)
      val {get = getVectorEqualFunc: Type.t -> Func.t option,
           set = setVectorEqualFunc,
           destroy = destroyVectorEqualFunc} =
         Property.destGetSet (Type.plist, Property.initConst NONE)
      val (getIntInfEqualFunc: unit -> Func.t option,
           setIntInfEqualFunc: Func.t option -> unit) =
         let
            val r = ref NONE
         in
            (fn () => !r, fn fo => r := fo)
         end
      val returns = SOME (Vector.new1 Type.bool)
      val seqIndexWordSize = WordSize.seqIndex ()
      val seqIndexTy = Type.word seqIndexWordSize
      val newFunctions: Function.t list ref = ref []
      fun newFunction z =
         List.push (newFunctions,
                    Function.profile (Function.new z,
                                      SourceInfo.polyEqual))
      fun equalTyconFunc (tycon: Tycon.t): Func.t =
         case getTyconEqualFunc tycon of
            SOME f => f
          | NONE =>
               let
                  val name =
                     Func.newString (concat ["equal_", Tycon.originalName tycon])
                  val _ = setTyconEqualFunc (tycon, SOME name)
                  val ty = Type.datatypee tycon
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
      and mkVectorEqualFunc {name: Func.t,
                             ty: Type.t, doEq: bool}: unit =
         let
            val loop = Func.newString (Func.originalName name ^ "Loop")
            (* Build two functions, one that checks the lengths and the
             * other that loops.
             *)
            val vty = Type.vector ty
            local
               val vec1 = (Var.newNoname (), vty)
               val vec2 = (Var.newNoname (), vty)
               val args = Vector.new2 (vec1, vec2)
               val dvec1 = Dexp.var vec1
               val dvec2 = Dexp.var vec2
               val len1 = (Var.newNoname (), seqIndexTy)
               val dlen1 = Dexp.var len1
               val len2 = (Var.newNoname (), seqIndexTy)
               val dlen2 = Dexp.var len2

               val body =
                  let
                     fun length dvec =
                        Dexp.primApp {prim = Prim.vectorLength,
                                      targs = Vector.new1 ty,
                                      args = Vector.new1 dvec,
                                      ty = Type.word seqIndexWordSize}
                     val body =
                        Dexp.lett
                        {decs = [{var = #1 len1, exp = length dvec1},
                                 {var = #1 len2, exp = length dvec2}],
                         body =
                         Dexp.conjoin
                         (Dexp.wordEqual (dlen1, dlen2, seqIndexWordSize),
                          Dexp.call
                          {func = loop,
                           args = Vector.new4
                                  (dvec1, dvec2, dlen1,
                                   Dexp.word (WordX.zero seqIndexWordSize)),
                           ty = Type.bool})}
                  in
                     if doEq
                        then Dexp.disjoin (Dexp.eq (dvec1, dvec2, vty), body)
                     else body
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
               val vec1 = (Var.newNoname (), vty)
               val vec2 = (Var.newNoname (), vty)
               val len = (Var.newNoname (), seqIndexTy)
               val i = (Var.newNoname (), seqIndexTy)
               val args = Vector.new4 (vec1, vec2, len, i)
               val dvec1 = Dexp.var vec1
               val dvec2 = Dexp.var vec2
               val dlen = Dexp.var len
               val di = Dexp.var i
               val body =
                  let
                     fun sub (dvec, di) =
                        Dexp.primApp {prim = Prim.vectorSub,
                                      targs = Vector.new1 ty,
                                      args = Vector.new2 (dvec, di),
                                      ty = ty}
                     val args =
                        Vector.new4
                        (dvec1, dvec2, dlen,
                         Dexp.add
                         (di, Dexp.word (WordX.one seqIndexWordSize),
                          seqIndexWordSize))
                  in
                     Dexp.disjoin
                     (Dexp.wordEqual
                      (di, dlen, seqIndexWordSize),
                      Dexp.conjoin
                      (equalExp (sub (dvec1, di), sub (dvec2, di), ty),
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
            ()
         end
      and vectorEqualFunc (ty: Type.t): Func.t =
         case getVectorEqualFunc ty of
            SOME f => f
          | NONE =>
               let
                  val name = Func.newString "vectorEqual"
                  val _ = setVectorEqualFunc (ty, SOME name)
                  val () = mkVectorEqualFunc {name = name, ty = ty, doEq = true}
               in
                  name
               end
      and intInfEqualFunc (): Func.t =
         case getIntInfEqualFunc () of
            SOME f => f
          | NONE =>
               let
                  val intInfEqual = Func.newString "intInfEqual"
                  val _ = setIntInfEqualFunc (SOME intInfEqual)

                  val bws = WordSize.bigIntInfWord ()
                  val sws = WordSize.smallIntInfWord ()

                  val bigIntInfEqual = Func.newString "bigIntInfEqual"
                  val () = mkVectorEqualFunc {name = bigIntInfEqual,
                                              ty = Type.word bws,
                                              doEq = false}

                  local
                     val arg1 = (Var.newNoname (), Type.intInf)
                     val arg2 = (Var.newNoname (), Type.intInf)
                     val args = Vector.new2 (arg1, arg2)
                     val darg1 = Dexp.var arg1
                     val darg2 = Dexp.var arg2
                     fun toWord dx =
                        Dexp.primApp
                        {prim = Prim.intInfToWord,
                         targs = Vector.new0 (),
                         args = Vector.new1 dx,
                         ty = Type.word sws}
                     fun toVector dx =
                        Dexp.primApp
                        {prim = Prim.intInfToVector,
                         targs = Vector.new0 (),
                         args = Vector.new1 dx,
                         ty = Type.vector (Type.word bws)}
                     val one = Dexp.word (WordX.one sws)
                     val body =
                        Dexp.disjoin
                        (Dexp.eq (darg1, darg2, Type.intInf),
                         Dexp.casee
                         {test = Dexp.wordEqual (Dexp.andb (Dexp.orb (toWord darg1, toWord darg2, sws), one, sws), one, sws),
                          ty = Type.bool,
                          default = NONE,
                          cases =
                          (Dexp.Con o Vector.new2)
                          ({con = Con.truee,
                            args = Vector.new0 (),
                            body = Dexp.falsee},
                           {con = Con.falsee,
                            args = Vector.new0 (),
                            body =
                            Dexp.call {func = bigIntInfEqual,
                                       args = Vector.new2 (toVector darg1, toVector darg2),
                                       ty = Type.bool}})})
                     val (start, blocks) = Dexp.linearize (body, Handler.Caller)
                     val blocks = Vector.fromList blocks
                  in
                     val _ =
                        newFunction {args = args,
                                     blocks = blocks,
                                     mayInline = true,
                                     name = intInfEqual,
                                     raises = NONE,
                                     returns = returns,
                                     start = start}
                  end
               in
                  intInfEqual
               end
      and equalExp (e1: Dexp.t, e2: Dexp.t, ty: Type.t): Dexp.t =
         Dexp.name (e1, fn x1 => 
         Dexp.name (e2, fn x2 => equal (x1, x2, ty)))
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
                  else Dexp.call {func = equalTyconFunc tycon,
                                  args = Vector.new2 (dx1, dx2),
                                  ty = Type.bool}
             | Type.IntInf => 
                  if hasConstArg ()
                     then eq ()
                  else Dexp.call {func = intInfEqualFunc (),
                                  args = Vector.new2 (dx1, dx2),
                                  ty = Type.bool}
             | Type.Real rs =>
                  let
                     val ws = WordSize.fromBits (RealSize.bits rs)
                     fun toWord dx =
                        Dexp.primApp
                        {prim = Prim.realCastToWord (rs, ws),
                         targs = Vector.new0 (),
                         args = Vector.new1 dx,
                         ty = Type.word ws}
                  in
                     Dexp.wordEqual (toWord dx1, toWord dx2, ws)
                  end
             | Type.Ref _ => eq ()
             | Type.Thread => eq ()
             | Type.Tuple tys =>
                  let
                     val max = Vector.length tys - 1
                     (* test components i, i+1, ... *)
                     fun loop (i: int): Dexp.t =
                        if i > max
                           then Dexp.truee
                        else let
                                val ty = Vector.sub (tys, i)
                                fun select dx =
                                   Dexp.select {tuple = dx,
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
             | Type.Weak _ => eq ()
             | Type.Word ws => prim (Prim.wordEqual ws, Vector.new0 ())
         end

      val _ =
         Vector.foreach
         (datatypes, fn Datatype.T {tycon, cons} =>
          setTyconInfo (tycon,
                        {isEnum = Vector.forall (cons, fn {args, ...} =>
                                                 Vector.isEmpty args),
                         cons = cons}))
      fun setBind (Statement.T {exp, var, ...}) =
         let
            fun const () =
               case var of
                  NONE => ()
                | SOME x => setVarInfo (x, {isConst = true})
         in
            case exp of
               Const c =>
                  (case c of
                      Const.IntInf i =>
                         (case Const.IntInfRep.fromIntInf i of
                             Const.IntInfRep.Big _ => ()
                           | Const.IntInfRep.Small _ => const ())
                    | Const.Word _ => const ()
                    | _ => ())
             | ConApp {args, ...} =>
                  if Vector.isEmpty args then const () else ()
             | _ => ()
         end
      val _ = Vector.foreach (globals, setBind)
      val () =
         List.foreach
         (functions, fn f =>
          let
             val {name, blocks, ...} = Function.dest f
          in
             Vector.foreach
             (blocks, fn Block.T {label, statements, ...} =>
              let
                 fun setHasEqual () =
                    (setFuncInfo (name, {hasEqual = true})
                     ; setLabelInfo (label, {hasEqual = true}))
              in
                 Vector.foreach
                 (statements, fn stmt as Statement.T {exp, ...} =>
                  (setBind stmt;
                   case exp of
                      PrimApp {prim, ...} =>
                         (case Prim.name prim of
                             Prim.Name.MLton_eq => setHasEqual ()
                           | Prim.Name.MLton_equal => setHasEqual ()
                           | _ => ())
                    | _ => ()))
              end)
          end)
      fun doit blocks =
         let
            val blocks = 
               Vector.fold
               (blocks, [], 
                fn (block as Block.T {label, args, statements, transfer}, blocks) =>
                if not (#hasEqual (labelInfo label))
                   then block::blocks
                else
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
                         fun adds ss = (blocks,
                                        {label = label,
                                         args = args,
                                         statements = ss @ statements})
                       in
                         case exp of
                            PrimApp {prim, targs, args, ...} =>
                               (case (Prim.name prim, Vector.length targs) of
                                   (Prim.Name.MLton_eq, 1) =>
                                      (case Type.dest (Vector.sub (targs, 0)) of
                                          Type.CPointer => 
                                             let
                                                val cp0 = Vector.sub (args, 0)
                                                val cp1 = Vector.sub (args, 1)
                                                val cpointerEqStmt =
                                                   Statement.T
                                                   {var = var,
                                                    ty = Type.bool,
                                                    exp = Exp.PrimApp
                                                          {prim = Prim.cpointerEqual,
                                                           targs = Vector.new0 (),
                                                           args = Vector.new2 (cp0,cp1)}}
                                             in
                                                adds [cpointerEqStmt]
                                             end
                                        | Type.Real rs =>
                                             let
                                                val ws = WordSize.fromBits (RealSize.bits rs)
                                                val wt = Type.word ws
                                                val r0 = Vector.sub (args, 0)
                                                val r1 = Vector.sub (args, 1)
                                                val w0 = Var.newNoname ()
                                                val w1 = Var.newNoname ()
                                                fun realCastToWordStmt (r, w) =
                                                   Statement.T
                                                   {var = SOME w,
                                                    ty = wt,
                                                    exp = Exp.PrimApp
                                                          {prim = Prim.realCastToWord (rs, ws),
                                                           targs = Vector.new0 (),
                                                           args = Vector.new1 r}}
                                                val wordEqStmt =
                                                   Statement.T
                                                   {var = var,
                                                    ty = Type.bool,
                                                    exp = Exp.PrimApp
                                                          {prim = Prim.wordEqual ws,
                                                           targs = Vector.new0 (),
                                                           args = Vector.new2 (w0,w1)}}
                                             in
                                                adds [wordEqStmt, 
                                                      realCastToWordStmt (r1, w1),
                                                      realCastToWordStmt (r0, w0)]
                                             end
                                        | Type.Word ws =>
                                             let
                                                val w0 = Vector.sub (args, 0)
                                                val w1 = Vector.sub (args, 1)
                                                val wordEqStmt =
                                                   Statement.T
                                                   {var = var,
                                                    ty = Type.bool,
                                                    exp = Exp.PrimApp
                                                          {prim = Prim.wordEqual ws,
                                                           targs = Vector.new0 (),
                                                           args = Vector.new2 (w0,w1)}}
                                             in
                                                adds [wordEqStmt]
                                             end
                                        | _ => normal ())
                                 | (Prim.Name.MLton_equal, 1) =>
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
             val f =
                if #hasEqual (funcInfo name)
                   then Function.new {args = args,
                                      blocks = doit blocks,
                                      mayInline = mayInline,
                                      name = name,
                                      raises = raises,
                                      returns = returns,
                                      start = start}
                else f
             val () = Function.clear f
          in
             f
          end)
      val program =
         Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = (!newFunctions) @ functions,
                    main = main}
      val _ = destroyVectorEqualFunc ()
      val _ = Program.clearTop program
   in
      program
   end

end
