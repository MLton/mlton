(* Copyright (C) 2009,2011,2017,2019,2022,2025 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Shrink (S: SHRINK_STRUCTS): SHRINK = 
struct

open S

structure Exp =
   struct
      open Exp

      val isProfile =
         fn Profile _ => true
          | _ => false
   end

structure Statement =
   struct
      open Statement

      fun isProfile (T {exp, ...}) = Exp.isProfile exp
   end

structure Array =
   struct
      open Array

      fun inc (a: int t, i: int): unit = update (a, i, 1 + sub (a, i))
   end

datatype z = datatype Exp.t
datatype z = datatype Transfer.t

structure VarInfo =
   struct
      datatype t = T of {isUsed: bool ref,
                         numOccurrences: int ref,
                         ty: Type.t option,
                         value: value option ref,
                         var: Var.t}
      and value =
         Con of {con: Con.t,
                 args: t vector}
        | Const of Const.t
        | Select of {tuple: t, offset: int}
        | Tuple of t vector

      fun equals (T {var = x, ...}, T {var = y, ...}) = Var.equals (x, y)

      fun layout (T {isUsed, numOccurrences, ty, value, var}) =
         let open Layout
         in record [("isUsed", Bool.layout (!isUsed)),
                    ("numOccurrences", Int.layout (!numOccurrences)),
                    ("ty", Option.layout Type.layout ty),
                    ("value", Option.layout layoutValue (!value)),
                    ("var", Var.layout var)]
         end
      and layoutValue v =
         let open Layout
         in case v of
            Con {con, args} => seq [Con.layout con,
                                    Vector.layout layout args]
          | Const c => Const.layout c
          | Select {tuple, offset} => seq [str "#", Int.layout (offset + 1), 
                                           str " ", layout tuple]
          | Tuple vis => Vector.layout layout vis
         end

      fun new (x: Var.t, ty: Type.t option) = T {isUsed = ref false,
                                                 numOccurrences = ref 0,
                                                 ty = ty,
                                                 value = ref NONE,
                                                 var = x}

      fun setValue (T {value, ...}, v) =
         (Assert.assert ("Ssa.Shrink.VarInfo.setValue", fn () => Option.isNone (!value))
          ; value := SOME v)


      fun numOccurrences (T {numOccurrences = r, ...}) = r
      fun ty (T {ty, ...}): Type.t option = ty
      fun value (T {value, ...}): value option = !value
      fun var (T {var, ...}): Var.t = var
   end

structure Value =
   struct
      datatype t = datatype VarInfo.value
   end

structure Position =
   struct
      datatype t =
         Formal of int
       | Free of Var.t

      fun layout (p: t) =
         case p of
            Formal i => Int.layout i
          | Free x => Var.layout x

      val equals =
         fn (Formal i, Formal i') => i = i'
          | (Free x, Free x') => Var.equals (x, x')
          | _ => false
   end

structure Positions = MonoVector (Position)

structure LabelMeaning =
   struct
      datatype t = T of {aux: aux,
                         blockIndex: int, (* The index of the block *)
                         label: Label.t} (* redundant, the label of the block *)

      and aux =
         Block
       | Bug
       | Case of {cases: (Con.t, Label.t) Cases.t,
                  default: Label.t option,
                  profileStmts: Statement.t list}
       | Goto of {dst: t,
                  args: Positions.t,
                  profileStmts: Statement.t list}
       | Raise of {args: Positions.t,
                   profileStmts: Statement.t list}
       | Return of {args: Positions.t,
                    profileStmts: Statement.t list}

      local
         fun make f (T r) = f r
      in
         val aux = make #aux
         val blockIndex = make #blockIndex
      end

      fun layout (T {aux, label, ...}) =
         let
            open Layout
         in
            seq [Label.layout label,
                 str " ",
                 case aux of
                    Block => str "Block "
                  | Bug => str "Bug"
                  | Case _ => str "Case"
                  | Goto {dst, args, ...} =>
                       seq [str "Goto ",
                            tuple [layout dst, Positions.layout args]]
                  | Raise {args, ...} =>
                       seq [str "Raise ", Positions.layout args]
                  | Return {args, ...} =>
                       seq [str "Return ", Positions.layout args]]
         end
   end

structure State =
   struct
      datatype state =
         Unvisited
       | Visited of LabelMeaning.t
       | Visiting

      val layout =
         let
            open Layout
         in
            fn Unvisited => str "Unvisited"
             | Visited m => LabelMeaning.layout m
             | Visiting => str "Visiting"
         end
   end

val traceApplyInfo = Trace.info "Ssa.Shrink.Prim.apply"

fun shrinkFunction {globals: Statement.t vector} =
   let
      fun use (VarInfo.T {isUsed, var, ...}): Var.t =
         (isUsed := true
          ; var)
      fun uses (vis: VarInfo.t vector): Var.t vector = Vector.map (vis, use)
      (* varInfo can't be getSetOnce because of setReplacement. *)
      val {get = varInfo: Var.t -> VarInfo.t, set = setVarInfo, ...} =
         Property.getSet (Var.plist, 
                          Property.initFun (fn x => VarInfo.new (x, NONE)))
(*       Property.getSet (Var.plist, Property.initFun VarInfo.new) *)
      val setVarInfo =
         Trace.trace2 ("Ssa.Shrink.setVarInfo",
                       Var.layout, VarInfo.layout, Unit.layout)
         setVarInfo
      fun varInfos xs = Vector.map (xs, varInfo)
      fun simplifyVar (x: Var.t) = use (varInfo x)
      val simplifyVar =
         Trace.trace ("Ssa.Shrink.simplifyVar", Var.layout, Var.layout) simplifyVar
      fun simplifyVars xs = Vector.map (xs, simplifyVar)
      fun incVarInfo (x: VarInfo.t): unit =
         Int.inc (VarInfo.numOccurrences x)
      fun incVar (x: Var.t): unit = incVarInfo (varInfo x)
      fun incVars xs = Vector.foreach (xs, incVar)
      fun numVarOccurrences (x: Var.t): int =
         ! (VarInfo.numOccurrences (varInfo x))
      val _ =
         Vector.foreach
         (globals, fn Statement.T {var, exp, ty} =>
          let
             val _ = Option.app
                     (var, fn x =>
                      setVarInfo (x, VarInfo.new (x, SOME ty)))
             fun construct v =
                Option.app (var, fn x => VarInfo.setValue (varInfo x, v))
          in case exp of
             ConApp {con, args} =>
                construct (Value.Con {con = con,
                                      args = Vector.map (args, varInfo)})
           | Const c => construct (Value.Const c)
           | Select {tuple, offset} =>
                construct (Value.Select {tuple = varInfo tuple,
                                         offset = offset})
           | Tuple xs => construct (Value.Tuple (Vector.map (xs, varInfo)))
           | Var y => Option.app (var, fn x => setVarInfo (x, varInfo y))
           | _ => ()
          end)
   in
      fn f: Function.t =>
      let
         val _ = Function.clear f
         val {args, blocks, mayInline, name, raises, returns, start, ...} =
            Function.dest f
         val _ = Vector.foreach
                 (args, fn (x, ty) => 
                  setVarInfo (x, VarInfo.new (x, SOME ty)))
         (* Index the labels by their defining block in blocks. *)
         val {get = labelIndex, set = setLabelIndex, ...} =
            Property.getSetOnce (Label.plist,
                                 Property.initRaise ("index", Label.layout))
         val _ = Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
                                  setLabelIndex (label, i))
         val numBlocks = Vector.length blocks
         (* Do a DFS to compute occurrence counts and set label meanings *)
         val states = Array.array (numBlocks, State.Unvisited)
         val inDegree = Array.array (numBlocks, 0)
         fun addLabelIndex i = Array.inc (inDegree, i)
         val isHeader = Array.array (numBlocks, false)
         fun layoutLabel (l: Label.t): Layout.t =
            let
               val i = labelIndex l
            in
               Layout.record [("label", Label.layout l),
                              ("inDegree", Int.layout (Array.sub (inDegree, i)))]
            end
         fun incAux aux =
            case aux of
               LabelMeaning.Goto {dst, ...} =>
                  addLabelIndex (LabelMeaning.blockIndex dst)
             | _ => ()
         fun incLabel (l: Label.t): unit =
            incLabelMeaning (labelMeaning l)
         and incLabelMeaning (LabelMeaning.T {aux, blockIndex, ...}): unit =
            let
               val i = blockIndex
               val n = Array.sub (inDegree, i)
               val _ = Array.update (inDegree, i, 1 + n)
            in
               if n = 0
                  then incAux aux
               else ()
            end
         and labelMeaning (l: Label.t): LabelMeaning.t =
            let
               val i = labelIndex l
            in
               case Array.sub (states, i) of
                  State.Visited m => m
                | State.Visiting =>
                     (Array.update (isHeader, i, true)
                      ; (LabelMeaning.T
                         {aux = LabelMeaning.Block,
                          blockIndex = i,
                          label = Block.label (Vector.sub (blocks, i))}))
                | State.Unvisited => 
                     let
                        val _ = Array.update (states, i, State.Visiting)
                        val m = computeMeaning i
                        val _ = Array.update (states, i, State.Visited m)
                     in
                        m
                     end
            end
         and computeMeaning (i: int): LabelMeaning.t =
            let
               val Block.T {args, statements, transfer, ...} =
                  Vector.sub (blocks, i)
               val _ =
                  Vector.foreach (args, fn (x, ty) =>
                                  setVarInfo (x, VarInfo.new (x, SOME ty)))
               val _ =
                  Vector.foreach
                  (statements, fn s => Exp.foreachVar (Statement.exp s, incVar))
               fun extract (actuals: Var.t vector): Positions.t =
                  let
                     val {get: Var.t -> Position.t, set, destroy} =
                        Property.destGetSetOnce
                        (Var.plist, Property.initFun Position.Free)
                     val _ = Vector.foreachi (args, fn (i, (x, _)) =>
                                              set (x, Position.Formal i))
                     val ps = Vector.map (actuals, get)
                     val _ = destroy ()
                  in ps
                  end
               fun doit aux =
                  LabelMeaning.T {aux = aux,
                                  blockIndex = i,
                                  label = Block.label (Vector.sub (blocks, i))}
               fun normal () = doit LabelMeaning.Block
               fun profileStmts () =
                  Vector.toListMap
                  (statements, fn Statement.T {exp, ...} =>
                   if Exp.isProfile exp
                      then Statement.T {exp = exp, ty = Type.unit, var = NONE}
                   else Error.bug "Ssa.Shrink.computeMeaning.profileStmts: not Exp.isProfile")
               fun rr (xs: Var.t vector, make) =
                  let
                     val _ = incVars xs
                  in
                     if Vector.forall (statements, Statement.isProfile)
                        (* andalso (Vector.isEmpty xs *)
                        (*          orelse not (Vector.isEmpty args)) *)
                        then doit (make {args = extract xs,
                                         profileStmts = profileStmts ()})
                     else normal ()
                  end
            in
               case transfer of
                  Bug =>
                     if Vector.forall (statements, Statement.isProfile)
                        andalso (case returns of
                                    NONE => true
                                  | SOME ts =>
                                       Vector.equals
                                       (ts, args, fn (t, (_, t')) =>
                                        Type.equals (t, t')))
                        then doit LabelMeaning.Bug
                     else normal ()
                | Call {args, return, ...} =>
                     let
                        val _ = incVars args
                        val _ = Return.foreachLabel (return, incLabel)
                     in
                        normal ()
                     end
                | Case {test, cases, default} =>
                     let
                        val _ = incVar test
                        val _ = Cases.foreach (cases, incLabel)
                        val _ = Option.app (default, incLabel)
                     in
                        if Vector.forall (statements, Statement.isProfile)
                           andalso not (Array.sub (isHeader, i))
                           andalso 1 = Vector.length args
                           andalso 1 = numVarOccurrences test
                           andalso Var.equals (test, #1 (Vector.first args))
                           then
                              doit (LabelMeaning.Case {cases = cases,
                                                       default = default,
                                                       profileStmts = profileStmts ()})
                        else
                           normal ()
                     end
                | Goto {dst, args = actuals} =>
                     let
                        val _ = incVars actuals
                        val m = labelMeaning dst
                     in
                        if Vector.exists (statements, not o Statement.isProfile)
                           orelse Array.sub (isHeader, i)
                           then (incLabelMeaning m
                                 ; normal ())
                        else
                           if Vector.isEmpty statements
                              andalso
                              Vector.equals (args, actuals, fn ((x, _), x') =>
                                             Var.equals (x, x')
                                             andalso 1 = numVarOccurrences x)
                              then m (* It's an eta. *)
                           else
                           let
                              val ps = extract actuals
                              val n =
                                 Vector.fold (args, 0, fn ((x, _), n) =>
                                              n + numVarOccurrences x)
                              val n' =
                                 Vector.fold (ps, 0, fn (p, n) =>
                                              case p of
                                                 Position.Formal _ => n + 1
                                               | _ => n)
                              datatype z = datatype LabelMeaning.aux
                           in
                              if n <> n'
                                 then (incLabelMeaning m
                                       ; normal ())
                              else
                                 let
                                    fun extract (ps': Positions.t)
                                       : Positions.t =
                                       Vector.map
                                       (ps', fn p =>
                                        let
                                           datatype z = datatype Position.t
                                        in
                                           case p of
                                              Free x => Free x
                                            | Formal i => Vector.sub (ps, i)
                                        end)
                                    val profileStmts' = profileStmts ()
                                    val a =
                                       case LabelMeaning.aux m of
                                          Block =>
                                             Goto {dst = m,
                                                   args = ps,
                                                   profileStmts = profileStmts'}
                                        | Bug =>
                                             if (case returns of
                                                    NONE => true
                                                  | SOME ts =>
                                                       Vector.equals
                                                       (ts, args, fn (t, (_, t')) =>
                                                        Type.equals (t, t')))
                                                then Bug
                                             else Goto {dst = m,
                                                        args = ps,
                                                        profileStmts = profileStmts'}
                                        | Case _ => 
                                             Goto {dst = m,
                                                   args = ps,
                                                   profileStmts = profileStmts'}
                                        | Goto {dst, args, profileStmts} =>
                                             Goto {dst = dst,
                                                   args = extract args,
                                                   profileStmts = profileStmts' @ profileStmts}
                                        | Raise {args, profileStmts} =>
                                             Raise {args = extract args,
                                                    profileStmts = profileStmts' @ profileStmts}
                                        | Return {args, profileStmts} =>
                                             Return {args = extract args,
                                                     profileStmts = profileStmts' @ profileStmts}
                                 in
                                    doit a
                                 end
                           end
                     end
                | Raise xs => rr (xs, LabelMeaning.Raise)
                | Return xs => rr (xs, LabelMeaning.Return)
                | Runtime {args, return, ...} =>
                     (incVars args
                      ; incLabel return
                      ; normal ())
            end
         val _ = incLabel start
         fun indexMeaning i =
            case Array.sub (states, i) of
               State.Visited m => m
             | _ => Error.bug "Ssa.Shrink.indexMeaning: not computed"
         val indexMeaning =
            Trace.trace ("Ssa.Shrink.indexMeaning", Int.layout, LabelMeaning.layout)
            indexMeaning
         val labelMeaning = indexMeaning o labelIndex
         val labelMeaning =
            Trace.trace ("Ssa.Shrink.labelMeaning",
                         Label.layout, LabelMeaning.layout)
            labelMeaning
         fun meaningLabel m =
            Block.label (Vector.sub (blocks, LabelMeaning.blockIndex m))
         fun labelArgs l =
            Block.args (Vector.sub (blocks, labelIndex l))
         fun meaningArgs m =
            Block.args (Vector.sub (blocks, LabelMeaning.blockIndex m))
         fun save (f, s) =
            let
               val {destroy, controlFlowGraph, ...} =
                  Function.layoutDot (f, Var.layout)
            in
               File.withOut
               (concat ["/tmp/", Func.toString (Function.name f),
                        ".", s, ".dot"],
                fn out => Layout.outputl (controlFlowGraph, out))
               ; destroy ()
            end
         val _ = if true then () else save (f, "pre")
         (* *)
         val _ =
            if true
               then ()
            else
               Layout.outputl
               (Vector.layout
                (fn i =>
                 (Layout.record
                  [("label",
                    Label.layout (Block.label (Vector.sub (blocks, i)))),
                   ("inDegree", Int.layout (Array.sub (inDegree, i))),
                   ("state", State.layout (Array.sub (states, i)))]))
                (Vector.tabulate (numBlocks, fn i => i)),
                Out.error)
         val _ =
            Assert.assert
            ("Ssa.Shrink.labelMeanings", fn () =>
             let
                val inDegree' = Array.array (numBlocks, 0)
                fun bumpIndex i = Array.inc (inDegree', i)
                fun bumpMeaning m = bumpIndex (LabelMeaning.blockIndex m)
                val bumpLabel = bumpMeaning o labelMeaning
                fun doit (LabelMeaning.T {aux, blockIndex, ...}) =
                   let
                      datatype z = datatype LabelMeaning.aux
                   in
                      case aux of
                         Block =>
                            Transfer.foreachLabel
                            (Block.transfer (Vector.sub (blocks, blockIndex)),
                             bumpLabel)
                       | Bug => ()
                       | Case {cases, default, ...} =>
                            (Cases.foreach (cases, bumpLabel)
                             ; Option.app (default, bumpLabel))
                       | Goto {dst, ...} => bumpMeaning dst
                       | Raise _ => ()
                       | Return _ => ()
                   end
                val _ =
                   Array.foreachi
                   (states, fn (i, s) =>
                    if Array.sub (inDegree, i) > 0
                       then
                          (case s of
                              State.Visited m => doit m
                            | _ => ())
                    else ())
                val _ = bumpMeaning (labelMeaning start)
             in
                Array.equals (inDegree, inDegree', Int.equals)
                orelse
                let
                   val _ =
                      Layout.outputl
                      (Vector.layout
                       (fn i =>
                        (Layout.record
                         [("label",
                           Label.layout (Block.label (Vector.sub (blocks, i)))),
                          ("inDegree", Int.layout (Array.sub (inDegree, i))),
                          ("inDegree'", Int.layout (Array.sub (inDegree', i))),
                          ("state", State.layout (Array.sub (states, i)))]))
                       (Vector.tabulate (numBlocks, fn i => i)),
                       Out.error)
                in
                   false
                end
             end)
         val isBlock = Array.array (numBlocks, false)
         (* Functions for maintaining inDegree. *)
         val addLabelIndex =
            fn i =>
            (Assert.assert ("Ssa.Shrink.addLabelIndex", fn () =>
                            Array.sub (inDegree, i) > 0)
             ; addLabelIndex i)
         val addLabelMeaning = addLabelIndex o LabelMeaning.blockIndex
         fun layoutLabelMeaning m =
            Layout.record
            [("inDegree", Int.layout (Array.sub
                                      (inDegree, LabelMeaning.blockIndex m))),
             ("meaning", LabelMeaning.layout m)]
         val traceDeleteLabelMeaning =
            Trace.trace ("SSa.Shrink.deleteLabelMeaning",
                         layoutLabelMeaning, Unit.layout)
         fun deleteLabel l = deleteLabelMeaning (labelMeaning l)
         and deleteLabelMeaning arg: unit =
            traceDeleteLabelMeaning
            (fn (m: LabelMeaning.t) =>
            let
               val i = LabelMeaning.blockIndex m
               val n = Array.sub (inDegree, i) - 1
               val _ = Array.update (inDegree, i, n)
               val _ = Assert.assert ("Ssa.Shrink.deleteLabelMeaning", fn () => n >= 0)
            in
               if n = 0 (* andalso not (Array.sub (isBlock, i)) *)
                  then
                     let
                        datatype z = datatype LabelMeaning.aux
                     in
                        case LabelMeaning.aux m of
                           Block =>
                              Transfer.foreachLabel
                              (Block.transfer (Vector.sub (blocks, i)),
                               deleteLabel)
                         | Bug => ()
                         | Case {cases, default, ...} =>
                              (Cases.foreach (cases, deleteLabel)
                               ; Option.app (default, deleteLabel))
                         | Goto {dst, ...} => deleteLabelMeaning dst
                         | Raise _ => ()
                         | Return _ => ()
                     end
               else ()
            end) arg
         fun primApp (prim: Type.t Prim.t, args: VarInfo.t vector)
            : (Type.t, VarInfo.t) Prim.ApplyResult.t =
            let
               val args' =
                  Vector.map
                  (args, fn vi =>
                   case vi of
                      VarInfo.T {value = ref (SOME v), ...} =>
                         (case v of
                             Value.Con {con, args} =>
                                if Vector.isEmpty args
                                   then Prim.ApplyArg.Con {con = con,
                                                           hasArg = false}
                                else Prim.ApplyArg.Var vi
                           | Value.Const c => Prim.ApplyArg.Const c
                           | _ => Prim.ApplyArg.Var vi)
                    | _ => Prim.ApplyArg.Var vi)
            in
               Trace.traceInfo'
               (traceApplyInfo,
                fn (p, args, _) =>
                let
                   open Layout
                in
                   seq [Prim.layout p, str " ",
                        List.layout (Prim.ApplyArg.layout
                                     (Var.layout o VarInfo.var)) args]
                end,
                Prim.ApplyResult.layout (Var.layout o VarInfo.var))
               Prim.apply
               (prim, Vector.toList args', VarInfo.equals)
            end
         (* Another DFS, this time accumulating the new blocks. *)
         val traceForceMeaningBlock =
            Trace.trace ("Ssa.Shrink.forceMeaningBlock",
                        layoutLabelMeaning, Unit.layout)
         val traceSimplifyBlock =
            Trace.trace2 ("Ssa.Shrink.simplifyBlock",
                          List.layout Statement.layout,
                          layoutLabel o Block.label,
                          Layout.tuple2 (List.layout Statement.layout,
                                         Transfer.layout))
         val traceGotoMeaning =
            Trace.trace3
            ("Ssa.Shrink.gotoMeaning",
             List.layout Statement.layout,
             layoutLabelMeaning,
             Vector.layout VarInfo.layout,
             Layout.tuple2 (List.layout Statement.layout, Transfer.layout))
         val traceEvalStatement =
            Trace.trace
            ("Ssa.Shrink.evalStatement",
             Statement.layout,
             Layout.ignore: (Statement.t list -> Statement.t list) -> Layout.t)
         val traceSimplifyTransfer =
            Trace.trace ("Ssa.Shrink.simplifyTransfer",
                         Transfer.layout,
                         Layout.tuple2 (List.layout Statement.layout,
                                        Transfer.layout))
         val traceSimplifyCase =
            Trace.trace
            ("Ssa.Shrink2.simplifyCase",
             fn {cases, default, profileStmts, test, ...} =>
             Layout.record [("cantSimplify", Layout.str "fn () => ..."),
                            ("gone", Layout.str "fn () => ..."),
                            ("profileStmts", List.layout Statement.layout profileStmts),
                            ("test/cases/default",
                             (Transfer.layout o Transfer.Case)
                             {cases = cases,
                              default = default,
                              test = VarInfo.var test})],
             Layout.tuple2 (List.layout Statement.layout, Transfer.layout))
         val newBlocks = ref []
         fun simplifyLabel l =
            let
               val m = labelMeaning l
               val _ = forceMeaningBlock m
            in
               meaningLabel m
            end
         and forceMeaningBlock arg =
            traceForceMeaningBlock
            (fn (LabelMeaning.T {aux, blockIndex = i, ...}) =>
             if Array.sub (isBlock, i)
                then ()
             else
                let
                   val _ = Array.update (isBlock, i, true)
                   val block as Block.T {label, args, ...} =
                      Vector.sub (blocks, i)
                   fun extract (p: Position.t): VarInfo.t =
                      varInfo (case p of
                                  Position.Formal n => #1 (Vector.sub (args, n))
                                | Position.Free x => x)
                   val (statements, transfer) =
                      let
                         fun rr ({args, profileStmts}, make) =
                            (profileStmts,
                             make (Vector.map (args, use o extract)))
                         datatype z = datatype LabelMeaning.aux
                      in
                         case aux of
                            Block => simplifyBlock ([], block)
                          | Bug => ([], Transfer.Bug)
                          | Case _ => simplifyBlock ([], block)
                          | Goto {dst, args, profileStmts} =>
                               gotoMeaning
                               (profileStmts,
                                dst,
                                Vector.map (args, extract))
                          | Raise z => rr (z, Transfer.Raise)
                          | Return z => rr (z, Transfer.Return)
                      end
                   val _ =
                      List.push
                      (newBlocks,
                       Block.T {label = label,
                                args = args,
                                statements = Vector.fromList statements,
                                transfer = transfer})
                in
                   ()
                end) arg
         and simplifyBlock arg : Statement.t list * Transfer.t =
            traceSimplifyBlock
            (fn (profileStmtsIn, Block.T {statements, transfer, ...}) =>
            let
               val f = evalStatements statements
               val (ss, transfer) = simplifyTransfer transfer
            in
               (profileStmtsIn @ (f ss), transfer)
            end) arg
         and evalStatements (ss: Statement.t vector)
            : Statement.t list -> Statement.t list =
            let
               val fs = Vector.map (ss, evalStatement)
            in
               fn ss => Vector.foldr (fs, ss, fn (f, ss) => f ss)
            end
         and simplifyTransfer arg : Statement.t list * Transfer.t =
            traceSimplifyTransfer
            (fn (t: Transfer.t) =>
            case t of
               Bug => ([], Bug)
             | Call {func, args, return} =>
                  let
                     val (statements, return) =
                        case return of
                           Return.NonTail {cont, handler} =>
                              let
                                 fun isEta (m: LabelMeaning.t,
                                            ps: Position.t vector): bool =
                                    Vector.length ps = Vector.length (meaningArgs m)
                                    andalso
                                    Vector.foralli
                                    (ps,
                                     fn (i, Position.Formal i') => i = i'
                                      | _ => false)
                                 val contMeaning = labelMeaning cont
                                 fun nonTail handler =
                                    let
                                       val _ = forceMeaningBlock contMeaning
                                       val handler =
                                          Handler.map
                                          (handler, fn l =>
                                           let
                                              val handlerMeaning = labelMeaning l
                                              val _ = forceMeaningBlock handlerMeaning
                                           in
                                              meaningLabel handlerMeaning
                                           end)
                                    in
                                       ([],
                                        Return.NonTail {cont = meaningLabel contMeaning,
                                                        handler = handler})
                                    end
                                 fun tail profileStatements =
                                    (deleteLabelMeaning contMeaning
                                     ; Handler.foreachLabel (handler, deleteLabel)
                                     ; (profileStatements, Return.Tail))
                                 fun cont (handler, handlerProfileStatements) =
                                    case LabelMeaning.aux contMeaning of
                                       LabelMeaning.Bug =>
                                          (case handlerProfileStatements of
                                              NONE => nonTail handler
                                            | SOME profileStmts => tail profileStmts)
                                     | LabelMeaning.Return {args, profileStmts} =>
                                          if isEta (contMeaning, args)
                                             then tail profileStmts
                                          else nonTail handler
                                     | _ => nonTail handler
                              in
                                 case handler of
                                    Handler.Caller => cont (handler, NONE)
                                  | Handler.Dead => cont (handler, NONE)
                                  | Handler.Handle l =>
                                       let
                                          val handlerMeaning = labelMeaning l
                                       in
                                          case LabelMeaning.aux handlerMeaning of
                                             LabelMeaning.Bug => cont (handler, NONE)
                                           | LabelMeaning.Raise {args, profileStmts} =>
                                                if isEta (handlerMeaning, args)
                                                   then cont (if List.isEmpty profileStmts
                                                                 then Handler.Caller
                                                                 else handler,
                                                              SOME profileStmts)
                                                else nonTail handler
                                           | _ => nonTail handler
                                       end
                              end
                         | _ => ([], return)
                  in 
                     (statements,
                      Call {func = func,
                            args = simplifyVars args,
                            return = return})
                  end
              | Case {test, cases, default} =>
                   let
                      val test = varInfo test
                      fun cantSimplify () =
                         ([],
                          Case {test = use test,
                                cases = Cases.map (cases, simplifyLabel),
                                default = Option.map (default, simplifyLabel)})
                   in
                      simplifyCase
                      {cantSimplify = cantSimplify,
                       cases = cases,
                       default = default,
                       gone = fn () => (Cases.foreach (cases, deleteLabel)
                                        ; Option.app (default, deleteLabel)),
                       profileStmts = [],
                       test = test}
                   end
              | Goto {dst, args} => goto (dst, varInfos args)
              | Raise xs => ([], Raise (simplifyVars xs))
              | Return xs => ([], Return (simplifyVars xs))
              | Runtime {prim, args, return} =>
                   ([], Runtime {prim = prim, 
                                 args = simplifyVars args, 
                                 return = simplifyLabel return})
                   ) arg
         and simplifyCase arg : Statement.t list * Transfer.t =
            traceSimplifyCase
            (fn {cantSimplify, cases, default, gone, profileStmts, test: VarInfo.t} =>
            let
               (* tryToEliminate makes sure that the destination meaning
                * hasn't already been simplified.  If it has, then we can't
                * simplify the case.
                *)
               fun tryToEliminate m =
                  let
                     val i = LabelMeaning.blockIndex m
                  in
                     if Array.sub (inDegree, i) = 0
                        then cantSimplify ()
                     else
                        let
                           val _ = addLabelIndex i
                           val _ = gone ()
                        in
                           gotoMeaning (profileStmts, m, Vector.new0 ())
                        end
                  end
            in
               if Cases.isEmpty cases
                  then (case default of
                           NONE => (profileStmts, Bug)
                         | SOME l => tryToEliminate (labelMeaning l))
               else
                  let
                     val l = Cases.hd cases
                     fun isOk (l': Label.t): bool = Label.equals (l, l')
                  in
                     if Vector.isEmpty (labelArgs l)
                        andalso Cases.forall (cases, isOk)
                        andalso (case default of
                                    NONE => true
                                  | SOME l => isOk l)
                        then
                           (* All cases the same -- eliminate the case. *)
                           tryToEliminate (labelMeaning l)
                     else
                        let
                           fun findCase (cases, isCon, args) =
                              let
                                 val n = Vector.length cases
                                 fun doit (l, args) =
                                    let
                                       val m = labelMeaning l
                                       val _ = addLabelMeaning m
                                       val _ = gone ()
                                    in
                                       gotoMeaning (profileStmts, m, args)
                                    end
                                 fun loop k =
                                    if k = n
                                       then
                                          (case default of
                                              NONE => (gone (); ([], Bug))
                                            | SOME l => doit (l, Vector.new0 ()))
                                    else
                                       let
                                          val (con, l) = Vector.sub (cases, k)
                                       in
                                          if isCon con
                                             then doit (l, args)
                                          else loop (k + 1)
                                       end
                              in
                                 loop 0
                              end
                        in
                           case (VarInfo.value test, cases) of
                              (SOME (Value.Const c), _) =>
                                 (case (cases, c) of
                                     (Cases.Word (_, cs), Const.Word w) =>
                                        findCase (cs,
                                                  fn w' => WordX.equals (w, w'),
                                                  Vector.new0 ())
                                   | _ =>
                                        Error.bug "Ssa.Shrink.simplifyCases: strange constant")
                            | (SOME (Value.Con {con, args}), Cases.Con cases) =>
                                 findCase (cases,
                                           fn c => Con.equals (con, c),
                                           args)
                            | _ => cantSimplify ()
                        end
                  end
            end) arg
         and goto (dst: Label.t, args: VarInfo.t vector)
            : Statement.t list * Transfer.t =
            gotoMeaning ([], labelMeaning dst, args)
         and gotoMeaning arg : Statement.t list * Transfer.t =
            traceGotoMeaning
            (fn (profileStmtsIn,
                 m as LabelMeaning.T {aux, blockIndex = i, ...},
                 args: VarInfo.t vector) =>
             let
                val n = Array.sub (inDegree, i)
                val _ = Assert.assert ("Ssa.Shrink.gotoMeaning", fn () => n >= 1)
                fun normal () =
                   if n = 1
                      then
                         let
                            val _ = Array.update (inDegree, i, 0)
                            val b = Vector.sub (blocks, i)
                            val _ =
                               Vector.foreach2
                               (Block.args b, args, fn ((x, _), vi) =>
                                setVarInfo (x, vi))
                         in
                            simplifyBlock (profileStmtsIn, b)
                         end
                   else
                      let
                         val _ = forceMeaningBlock m
                      in
                         (profileStmtsIn,
                          Goto {dst = Block.label (Vector.sub (blocks, i)),
                                args = uses args})
                      end
                fun extract p =
                   case p of
                      Position.Formal n => Vector.sub (args, n)
                    | Position.Free x => varInfo x
                fun rr ({args, profileStmts}, make) =
                   (profileStmtsIn @ profileStmts,
                    make (Vector.map (args, use o extract)))
                datatype z = datatype LabelMeaning.aux
             in
                case aux of
                   Block => normal ()
                 | Bug => ((*profileStmtsIn*)[], Transfer.Bug)
                 | Case {cases, default, profileStmts} =>
                      simplifyCase {cantSimplify = normal,
                                    cases = cases,
                                    default = default,
                                    gone = fn () => deleteLabelMeaning m,
                                    profileStmts = profileStmtsIn @ profileStmts,
                                    test = Vector.first args}
                 | Goto {dst, args, profileStmts} =>
                      if Array.sub (isHeader, i)
                         orelse Array.sub (isBlock, i)
                         then normal ()
                      else
                         let
                            val n' = n - 1
                            val _ = Array.update (inDegree, i, n')
                            val _ = 
                               if n' > 0
                                  then addLabelMeaning dst
                               else ()
                         in
                            gotoMeaning (profileStmtsIn @ profileStmts,
                                         dst, 
                                         Vector.map (args, extract))
                         end
                 | Raise z => rr (z, Transfer.Raise)
                 | Return z => rr (z, Transfer.Return)
             end) arg
         and evalStatement arg : Statement.t list -> Statement.t list =
            traceEvalStatement
            (fn (Statement.T {var, ty, exp}) =>
            let
               val _ = Option.app 
                       (var, fn x => 
                        setVarInfo (x, VarInfo.new (x, SOME ty)))
               fun delete ss = ss
               fun doit {makeExp: unit -> Exp.t,
                         sideEffect: bool,
                         value: Value.t option} =
                  let
                     fun make var =
                        Statement.T {var = var,
                                     ty = ty,
                                     exp = makeExp ()}
                  in
                     case var of
                        NONE =>
                           if sideEffect
                              then (fn ss => make NONE :: ss)
                           else delete
                      | SOME x =>
                           let
                              val VarInfo.T {isUsed, value = r, ...} = varInfo x
                              val _ = r := value
                           in
                              fn ss =>
                              if !isUsed
                                 then make (SOME x) :: ss
                              else if sideEffect
                                      then make NONE :: ss
                                   else ss
                           end
                  end
               fun setVar vi =
                  (Option.app (var, fn x => setVarInfo (x, vi))
                   ; delete)
               fun construct (v: Value.t, makeExp) =
                  doit {makeExp = makeExp,
                        sideEffect = false,
                        value = SOME v}
            in
               case exp of
                  ConApp {con, args} =>
                     let
                        val args = varInfos args
                     in
                        construct (Value.Con {con = con, args = args},
                                   fn () => ConApp {con = con,
                                                    args = uses args})
                     end
                | Const c => construct (Value.Const c, fn () => exp)
                | PrimApp {prim, targs, args} =>
                     let
                        val args = varInfos args
                        fun apply {prim, targs, args} =
                           doit {sideEffect = Prim.maySideEffect prim,
                                 makeExp = fn () => PrimApp {prim = prim,
                                                             targs = targs,
                                                             args = uses args},
                                 value = NONE}
                        datatype z = datatype Prim.ApplyResult.t
                     in
                        case primApp (prim, args) of
                           Apply (prim, args) => 
                              apply {prim = prim, targs = Vector.new0 (),
                                     args = Vector.fromList args}
                         | Bool b =>
                              let
                                 val con = Con.fromBool b
                              in
                                 construct (Value.Con {con = con,
                                                       args = Vector.new0 ()},
                                            fn () =>
                                            ConApp {con = con,
                                                    args = Vector.new0 ()})
                              end
                         | Const c => construct (Value.Const c,
                                                 fn () => Exp.Const c)
                         | Var vi => setVar vi
                         | _ => apply {prim = prim,
                                       targs = targs,
                                       args = args}
                     end
                | Select {tuple, offset} =>
                     let
                        val tuple as VarInfo.T {value, ...} = varInfo tuple
                     in
                        case !value of
                           SOME (Value.Tuple vs) =>
                              setVar (Vector.sub (vs, offset))
                         | _ =>
                              construct (Value.Select {tuple = tuple, 
                                                       offset = offset},
                                         fn () => Select {tuple = use tuple,
                                                          offset = offset})
                     end
                | Tuple xs =>
                     let
                        val xs = varInfos xs
                     in
                        case Exn.withEscape
                             (fn escape =>
                              Vector.foldri
                              (xs, NONE,
                               fn (i, VarInfo.T {value, ...}, tuple') => 
                               case !value of
                                  SOME (Value.Select {offset, tuple}) =>
                                     if offset = i
                                        then case tuple' of
                                                NONE => 
                                                   (case VarInfo.ty tuple of
                                                       SOME ty =>
                                                          (case Type.deTupleOpt ty of
                                                              SOME ts =>
                                                                 if Vector.length xs =
                                                                    Vector.length ts
                                                                    then SOME tuple
                                                                 else escape NONE
                                                            | NONE => escape NONE)
                                                     | NONE => escape NONE)
                                              | SOME tuple'' => 
                                                   if VarInfo.equals (tuple'', tuple)
                                                      then tuple'
                                                   else escape NONE
                                     else escape NONE
                                | _ => escape NONE)) of
                          SOME tuple => setVar tuple
                        | NONE => construct (Value.Tuple xs,
                                             fn () => Tuple (uses xs))
                     end
                | Var x => setVar (varInfo x)
                | _ => doit {makeExp = fn () => exp,
                             sideEffect = true,
                             value = NONE}
            end) arg
         val start = labelMeaning start
         val _ = forceMeaningBlock start
         val f = 
            Function.new {args = args,
                          blocks = Vector.fromList (!newBlocks),
                          mayInline = mayInline,
                          name = name,
                          raises = raises,
                          returns = returns,
                          start = meaningLabel start}
         val _ = if true then () else save (f, "post")
         val _ = Function.clear f
      in
         f
      end
   end

fun eliminateUselessProfile (f: Function.t): Function.t =
   if !Control.profile = Control.ProfileNone
      then f
   else
      let
         fun eliminateInBlock (b as Block.T {args, label, statements, transfer})
            : Block.t =
            if not (Vector.exists (statements, Statement.isProfile))
               then b
            else
               let
                  datatype z = datatype Exp.t
                  datatype z = datatype ProfileExp.t
                  val stack =
                     Vector.fold
                     (statements, [], fn (s as Statement.T {exp, ...}, stack) =>
                      case exp of
                         Profile (Leave si) =>
                            (case stack of
                                Statement.T {exp = Profile (Enter si'), ...}
                                :: rest =>
                                   if SourceInfo.equals (si, si')
                                      then rest
                                   else Error.bug "Ssa.Shrink.eliminateUselessProfile: mismatched Leave"
                              | _ => s :: stack)
                       | _ => s :: stack)
                  val statements = Vector.fromListRev stack
               in
                  Block.T {args = args,
                           label = label,
                           statements = statements,
                           transfer = transfer}
               end
         val {args, blocks, mayInline, name, raises, returns, start} =
            Function.dest f
         val blocks = Vector.map (blocks, eliminateInBlock)
      in
         Function.new {args = args,
                       blocks = blocks,
                       mayInline = mayInline,
                       name = name,
                       raises = raises,
                       returns = returns,
                       start = start}
      end

val traceShrinkFunction =
   Trace.trace ("Ssa.Shrink.shrinkFunction", Function.layout, Function.layout)

val shrinkFunction =
   fn g =>
   let
      val s = shrinkFunction g
   in
      fn f => traceShrinkFunction s (eliminateUselessProfile f)
   end

fun shrink (Program.T {datatypes, globals, functions, main})
  = let
      val s = shrinkFunction {globals = globals}
    in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = List.revMap (functions, s),
                 main = main}
    end

end
