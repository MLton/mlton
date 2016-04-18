(* Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Reduces or eliminates the iteration count of loops by duplicating
 * the loop body.
 *)
functor LoopUnroll(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer Prim

structure Graph = DirectedGraph
local
   open Graph
in
   structure Forest = LoopForest
end

val optCount = ref 0
val multiHeaders = ref 0
val varEntryArg = ref 0
val multiTransfer = ref 0
val nonGoto = ref 0
val ccTransfer = ref 0
val ccBound = ref 0
val infinite = ref 0

fun inc (v: int ref): unit =
  v := (!v) + 1

type BlockInfo = Label.t * (Var.t * Type.t) vector

structure Loop =
  struct
    datatype Bound = Eq of IntInf.t | Lt of IntInf.t | Gt of IntInf.t
    type Start = IntInf.t
    type Step = IntInf.t
    datatype t = T of {start: Start, step: Step, bound: Bound, invert: bool}

    fun toString (T {start, step, bound, invert}): string =
      let
        val boundStr = case bound of
          Eq b => if invert then
                    concat ["!= ", IntInf.toString b]
                  else
                    concat ["= ", IntInf.toString b]
        | Lt b => if invert then
                    concat ["!< ", IntInf.toString b]
                  else
                    concat ["< ", IntInf.toString b]
        | Gt b => if invert then
                    concat ["!> ", IntInf.toString b]
                  else
                    concat ["> ", IntInf.toString b]
      in
        concat[" Start: ", IntInf.toString start,
               " Step: ", IntInf.toString step,
               " Bound: ", boundStr]
      end
      

    fun isInfiniteLoop (T {start, step, bound, invert}): bool =
      case bound of
        Eq b =>
          if start = b then
            false
          else if start < b andalso step > 0 then
            not (((b - start) mod step) = 0)
          else if start > b andalso step < 0 then
            not (((start - b) mod (~step)) = 0)
          else
            true
      | Lt b => start < b andalso step <= 0
      | Gt b => start > b andalso step >= 0

    fun makeStatement (v: IntInf.t, wsize: WordSize.t): Var.t * Statement.t =
      let
        val newWord = WordX.fromIntInf (v, wsize)
        val newConst = Const.word newWord
        val newExp = Exp.Const (newConst)
        val newType = Type.word wsize
        val newVar = Var.newNoname()
        val newStatement = Statement.T {exp = newExp,
                                        ty = newType,
                                        var = SOME(newVar)}
      in
        (newVar, newStatement)
      end

    (* Assumes isInfiniteLoop is false. *)
    fun makeConstants (T {start, step, bound, invert},
                     wsize: WordSize.t)
                     : Var.t list * Statement.t list =
      case bound of
        Eq b =>
          if (not (start = b)) <> invert then
            let
              val (newVar, newStatement) = makeStatement(start, wsize)
              val nextIter = T {start = start + step,
                                step = step,
                                bound = bound,
                                invert = invert}
              val (rVars, rStmts) = makeConstants (nextIter, wsize)
            in
              (newVar::rVars, newStatement::rStmts)
            end
          else
            ([], [])
      | Lt b =>
          if (start < b) <> invert then
            let
              val (newVar, newStatement) = makeStatement(start, wsize)
              val nextIter = T {start = start + step,
                                step = step,
                                bound = bound,
                                invert = invert}
              val (rVars, rStmts) = makeConstants (nextIter, wsize)
            in
              (newVar::rVars, newStatement::rStmts)
            end
          else
            ([], [])
      | Gt b =>
          if (start > b) <> invert then
            let
              val (newVar, newStatement) = makeStatement(start, wsize)
              val nextIter = T {start = start + step,
                                step = step,
                                bound = bound,
                                invert = invert}
              val (rVars, rStmts) = makeConstants (nextIter, wsize)
            in
              (newVar::rVars, newStatement::rStmts)
            end
          else
            ([], [])
  end

fun logli (l: Layout.t, i: int): unit =
   Control.diagnostics
   (fn display =>
      display(Layout.indent(l, i)))

fun logl(l: Layout.t): unit =
   logli(l, 0)

fun logsi (s: string, i: int): unit =
   logli((Layout.str s), i)

fun logs (s: string): unit =
   logsi(s, 0)

fun vectorDrop (v, i) =
  Vector.concat [Vector.prefix(v, i),
                 Vector.dropPrefix(v, i + 1)]

fun listPop lst =
  case lst of
    [] => []
  | _::tl => tl

(* If a block was renamed, return the new name. Otherwise return the old name. *)
fun fixLabel (getBlockInfo: Label.t -> BlockInfo, 
              label: Label.t,
              origLabels: Label.t vector): Label.t =
  if Vector.contains(origLabels, label, Label.equals) then
    let
      val (name, _) = getBlockInfo(label)
    in
      name
    end
  else
    label

fun varOptEquals (v1: Var.t, v2: Var.t option): bool =
   case v2 of
     NONE => false
   | SOME (v2') => Var.equals (v1, v2')

(* For an binary operation where one argument is a constant,
   load that constant.
   Returns the variable, the constant, and true if the var was the first arg *)
fun varConst (args, loadVar) =
   let
      val a1 = Vector.sub (args, 0)
      val a2 = Vector.sub (args, 1)
      val a1v = loadVar(a1)
      val a2v = loadVar(a2)
   in
      case (a1v, a2v) of
        (SOME x, NONE) => SOME (a2, x, false)
      | (NONE, SOME x) => SOME (a1, x, true)
      | _ => NONE
   end

(* Compute the transformation from the entry var to the next entry var *)
(* TODO: Include arithmetic transfers *)
fun varChain (origVar, endVar, blocks, loadVar) =
   case Var.equals (origVar, endVar) of
     true => SOME (0)
   | false =>
      let
         val endVarAssign = Vector.peekMap (blocks, fn b =>
            let
               val stmts = Block.statements b
               val assignments = Vector.keepAllMap (stmts, fn s =>
                  case varOptEquals (endVar, Statement.var s) of
                    false => NONE
                  | true =>
                     (case Statement.exp s of
                        Exp.PrimApp {args, prim, ...} =>
                           (case Prim.name prim of
                              Name.Word_add _ =>
                                (case varConst(args, loadVar) of
                                  SOME(nextVar, x, _) => SOME (nextVar, x)
                                | NONE => NONE)
                            | Name.Word_addCheck _ =>
                                (case varConst(args, loadVar) of
                                  SOME(nextVar, x, _) => SOME(nextVar, x)
                                | NONE => NONE)
                            | Name.Word_sub _ =>
                                (case varConst(args, loadVar) of
                                  SOME(nextVar, x, _) => SOME (nextVar, ~x)
                                | NONE => NONE)
                            | Name.Word_subCheck _ =>
                                (case varConst(args, loadVar) of
                                  SOME(nextVar, x, _) => SOME (nextVar, ~x)
                                | NONE => NONE)
                            | _ => NONE)
                        | _ => NONE))
            in
               case Vector.length assignments of
                  0 => NONE
                | _ => SOME (Vector.sub (assignments, 0))
            end)
      in
         case endVarAssign of
           NONE => NONE
         | SOME (nextVar, x) =>
            (case varChain(origVar, nextVar, blocks, loadVar) of
               NONE => NONE
             | SOME (y) => SOME(x + y))
      end

(* Given a transfer on a boolean and a list of loop body labels, returns:
    - the label that exits the loop
    - the label that continues the loop
    - true if the continue branch is the true branch
 *)
fun loopExit (loopLabels: Label.t vector, transfer: Transfer.t): (Label.t * Label.t * bool) =
  case transfer of
    (* This should be a case statement on a boolean,
       so all dsts should be unary.
       One should transfer outside the loop, the other inside. *)
    Transfer.Case {cases, default, ...} =>
      (case default of
        SOME(defaultLabel) =>
          let
            val (caseCon, caseLabel) =
              case cases of
                Cases.Con v => Vector.sub (v, 0)
              (*| Cases.Word (_, v) =>
                  let
                    val (_, lbl) = Vector.sub (v, 0)
                  in
                    lbl
                  end*)
              | _ => raise Fail "This should be a con"
          in
            if Vector.contains (loopLabels, defaultLabel, Label.equals) then
              (caseLabel, defaultLabel, Con.equals (Con.fromBool false, caseCon))
            else
              (defaultLabel, caseLabel, Con.equals (Con.fromBool true, caseCon))
          end    
      | NONE =>
          (case cases of
            Cases.Con v =>
              let
                val (c1, d1) = Vector.sub (v, 0)
                val (c2, d2) = Vector.sub (v, 1)
              in
                if Vector.contains (loopLabels, d1, Label.equals) then
                  (d2, d1, Con.equals (Con.fromBool true, c1))
                else
                  (d1, d2, Con.equals (Con.fromBool true, c2))
              end
          (*| Cases.Word (_, v) =>
              let
                val (w, d1) = Vector.sub (v, 0)
                val (w1, d2) = Vector.sub (v, 1)
              in
                if Vector.contains (loopLabels, d1, Label.equals) then
                  (d2, d1)
                else
                  (d1, d2)
              end*)
            | _ => raise Fail "This should be a con"))
          
  | _ => raise Fail "This should be a case statement"

(* Check a loop phi value to see if it is an induction variable suitable for unrolling *)
fun checkArg ((argVar, _), argIndex, entryArg, header, loopBody, loadVar) =
   case entryArg of
      NONE => (logsi ("Can't unroll: entry arg not constant", 2) ;
               inc(varEntryArg) ;
               NONE)
   | SOME (entryX) =>
      let
         val headerLabel = Block.label header
         (* For every transfer to the start of the loop, get the variable at argIndex *)
         val nonGotoTransfer = ref false
         val loopVars = Vector.keepAllMap (loopBody, fn block => 
            case Block.transfer block of
               Transfer.Arith {success, ...} =>
                  if Label.equals (headerLabel, success) then
                     (nonGotoTransfer := true ; NONE)
                  else NONE
             | Transfer.Call {return, ...} =>
                  (case return of
                     Return.NonTail {cont, ...} =>
                        if Label.equals (headerLabel, cont) then
                           (nonGotoTransfer := true; NONE)
                        else NONE
                   | _ => NONE)
             | Transfer.Case {cases, ...} =>
                 (case cases of
                    Cases.Con v =>
                     if Vector.exists(v, fn (_, lbl) =>
                                           Label.equals (headerLabel, lbl)) then
                        (nonGotoTransfer := true ; NONE)
                     else NONE
                  | Cases.Word (_, v) =>
                     if Vector.exists(v, fn (_, lbl) =>
                                           Label.equals (headerLabel, lbl)) then
                        (nonGotoTransfer := true ; NONE)
                     else NONE)
             | Transfer.Goto {args, dst} =>
                  if Label.equals (headerLabel, dst) then
                     SOME (Vector.sub (args, argIndex))
                  else NONE
            | _ => NONE)
      in
         if (Vector.length loopVars) > 1 then
            (logsi ("Can't unroll: more than 1 transfer to head of loop", 2) ;
             inc(multiTransfer) ;
             NONE)
            (* TODO: This should only need to verify that all the variables are the same*)
         else if (!nonGotoTransfer) then
            (logsi ("Can't unroll: non-goto transfer to head of loop", 2) ;
             inc(nonGoto) ;
             NONE)
         else
            let
               val loopVar = Vector.sub (loopVars, 0)
            in
               case varChain (argVar, loopVar, loopBody, loadVar) of
                 NONE => (logsi ("Can't unroll: can't compute transfer", 2) ; 
                          inc(ccTransfer) ;
                          NONE)
               | SOME (step) =>
                  let
                    fun ltOrGt (vc) =
                      case vc of
                        NONE => NONE
                      | SOME (_, c, b) =>
                          if b then
                            SOME(Loop.Lt (c))
                          else
                            SOME(Loop.Gt (c))
                    fun eq (vc) =
                      case vc of
                        NONE => NONE
                      | SOME (_, c, _) => SOME(Loop.Eq (c))
                    (* TODO: This should look for a case statement anywhere *)
                     val headerTransferVar =
                       case Block.transfer header of
                         Transfer.Case {test, ...} => SOME(test)
                       | _ => NONE
                  in
                     case headerTransferVar of
                       NONE => (logsi ("Can't unroll: can't compute bound", 2) ;
                                inc(ccBound) ;
                                NONE)
                     | SOME (var) =>
                        let
                           val bound = Vector.peekMap (Block.statements header,
                              fn s => case Statement.var s of
                                NONE => NONE
                              | SOME (var') =>
                                if Var.equals (var, var') then
                                  case Statement.exp s of
                                    PrimApp {args, prim, ...} =>
                                      if not (Vector.contains (args, argVar, Var.equals))
                                      then
                                         NONE
                                      else
                                        (case Prim.name prim of
                                          Name.Word_lt _ => ltOrGt (varConst (args, loadVar))
                                        | Name.Word_equal _ => eq (varConst (args, loadVar))
                                        | _ => NONE)
                                  | _ => NONE
                                else NONE)

                        in
                           case bound of
                             NONE => NONE
                           | SOME(b) =>
                              let
                                val () = logsi ("Can unroll on this arg!", 2)
                                val loopLabels = Vector.map (loopBody, Block.label)
                                val (_, _, contIsTrue) =
                                      loopExit (loopLabels ,Block.transfer header)
                              in
                                SOME (argIndex,
                                      Loop.T {start = entryX,
                                              step = step,
                                              bound = b,
                                              invert = not contIsTrue})
                              end
                        end
                  end
            end
      end
(* Check all of a loop's entry point arguments to see if a constant value.
   Returns a list of int options where SOME(x) is always x for each entry. *)
fun findConstantStart (entryArgs: ((IntInf.t option) vector) vector):
                                                          (IntInf.t option) vector =
  if (Vector.length entryArgs) > 0 then                                                         
    Vector.rev (Vector.fold (entryArgs, Vector.sub (entryArgs, 0),
      fn (v1, v2) => Vector.fromList (Vector.fold2 (v1, v2, [], fn (a1, a2, lst) =>
        case (a1, a2) of
          (SOME(x1), SOME(x2)) =>
            if x1 = x2 then SOME(x1)::lst
            else NONE::lst
        | _ => NONE::lst))))
  else Vector.new0 ()

(* Look for any optimization opportunities in the loop. *)
fun findOpportunity(functionBody: Block.t vector,
                    loopBody: Block.t vector,
                    loopHeaders: Block.t vector,
                    loadGlobal: Var.t -> IntInf.t option,
                    depth: int):
                    (int * Loop.t) option =
   if (Vector.length loopHeaders) = 1 then
      let
         val header = Vector.sub (loopHeaders, 0)
         val headerArgs = Block.args header
         val headerLabel = Block.label header
         val () = logs "Evaluating loop with header"
         val () = logl (Label.layout headerLabel)
         fun blockEquals (b1, b2) = Label.equals (Block.label b1, Block.label b2)
         val emptyArgs = SOME(Vector.new (Vector.length headerArgs, NONE))
         val entryArgs = Vector.keepAllMap(functionBody, fn block =>
                          if Vector.contains (loopBody, block, blockEquals) then NONE
                          else case Block.transfer block of
                             Transfer.Arith {success, ...} =>
                              if Label.equals (headerLabel, success) then
                                 emptyArgs
                              else NONE
                           | Transfer.Call {return, ...} =>
                              (case return of
                                 Return.NonTail {cont, ...} =>
                                    if Label.equals (headerLabel, cont) then
                                       emptyArgs
                                    else NONE
                               | _ => NONE)
                           | Transfer.Case {cases, ...} =>
                              (case cases of
                                 Cases.Con v =>
                                    if Vector.exists (v, fn (_, lbl) =>
                                       Label.equals (headerLabel, lbl)) then
                                         emptyArgs
                                    else
                                       NONE
                               | Cases.Word (_, v) =>
                                    if Vector.exists (v, fn (_, lbl) =>
                                       Label.equals (headerLabel, lbl)) then
                                         emptyArgs
                                    else NONE)
                          | Transfer.Goto {args, dst} =>
                              if Label.equals (dst, headerLabel) then
                                SOME(Vector.map (args, loadGlobal))
                              else NONE
                          | _ => NONE)
         val () = logs (concat["Loop has ",
                               Int.toString (Vector.length entryArgs),
                               " entry points"])
         val constantArgs = findConstantStart entryArgs
         val () = logs "Got constant args"
         val () = Vector.foreach (constantArgs, fn a =>
                    case a of
                      NONE => logsi ("None", 1)
                    | SOME (v) => logsi (IntInf.toString v, 1))
         val unrollableArgs =
          Vector.keepAllMapi
            (headerArgs, fn (i, arg) => (
               logs "Checking arg:" ;
               logli (Var.layout (#1 arg), 1) ;
               checkArg (arg, i, Vector.sub (constantArgs, i),
                         header, loopBody, loadGlobal)))
      in
         if (Vector.length unrollableArgs) > 0 then
                  (logs "Found at least one unrollable argument" ;
                     SOME(Vector.sub (unrollableArgs, 0)))
         else NONE
      end
   else
      (logsi ("Can't optimize: loop has more than 1 header", depth) ;
       multiHeaders := (!multiHeaders) + 1 ;
       NONE)

fun makeHeader(oldHeader, argi, (newVars, newStmts), newEntry) =
  let
    val oldArgs = Block.args oldHeader
    val (_, oldType) = Vector.sub (oldArgs, argi)
    val argSize = case Type.dest oldType of
                    Type.Word wsize => wsize
                  | _ => raise Fail "Argument is not of type word"
    
    val newArgs = Vector.concat [Vector.prefix(oldArgs, argi),
                                 Vector.dropPrefix(oldArgs, argi + 1)]
    val newArgs' = Vector.map (newArgs, fn (arg, _) => arg)
    (*val () = logs("Old args")
    val () = Vector.foreach(oldArgs, fn (a, t) => logl(Var.layout a))
    val () = logs("New args")
    val () = Vector.foreach(newArgs, fn (a, t) => logl(Var.layout a))*)
    val newTransfer = Transfer.Goto {args = newArgs', dst = newEntry}
  in
    (Block.T {args = oldArgs,
              label = Block.label oldHeader,
              statements = Vector.fromList newStmts,
              transfer = newTransfer},
     newVars)
  end

(* Copy an entire loop. In the header, rewrite the transfer to take the loop branch.
   In the transfers to the top of the loop, rewrite the transfer to goto next.
   Ensure that the header is the first element in the list.
   Replace all instances of argi with argVar*)
fun copyLoop(blocks: Block.t vector,
             nextLabel: Label.t,
             headerLabel: Label.t,
             argi: int,
             argVar: Var.t,
             blockInfo: Label.t -> BlockInfo,
             setBlockInfo: Label.t * BlockInfo -> unit): Block.t vector =
  let
    val labels = Vector.map (blocks, Block.label)
    (* Assign a new label for each block *)
    val newBlocks = Vector.map (blocks, fn b =>
        let
          val oldName = Block.label b
          val oldArgs = Block.args b
          val newName = Label.newNoname()
          val () = setBlockInfo(oldName, (newName, oldArgs))
        in
          Block.T {args = Block.args b,
                   label = newName,
                   statements = Block.statements b,
                   transfer = Block.transfer b}
        end)
    (* Rewrite the transfers of each block *)
    val fixedBlocks = Vector.map (newBlocks,
                                  fn Block.T {args, label, statements, transfer} =>
      let
        val f = fn l => fixLabel(blockInfo, l, labels)
        val isHeader = Label.equals (label, f(headerLabel))
        val (newArgs, removedArg) =
          if isHeader then
            (vectorDrop(args, argi), SOME(Vector.sub (args, argi)))
          else (args, NONE)
        val newStmts =
          if isHeader then
            case removedArg of
              NONE => statements
            | SOME(var, ty) =>
                let
                  val assignExp = Exp.Var (argVar)
                  val assign = Statement.T {exp = assignExp,
                                            ty = ty,
                                            var = SOME(var)}
                  val assignV = Vector.new1(assign)
                in
                  Vector.concat [assignV, statements]
                end
          else
            statements
        val newTransfer =
          if isHeader then
            let
              val (_, contLabel, _) = loopExit(labels, transfer)
            in
              Transfer.Goto {args = Vector.new0 (), dst = f(contLabel)}
            end
          else
            case transfer of
              Transfer.Arith {args, overflow, prim, success, ty} =>
                Transfer.Arith {args = args,
                                overflow = f(overflow),
                                prim = prim,
                                success = f(success),
                                ty = ty}
            | Transfer.Call {args, func, return} =>
                let
                  val newReturn =
                    case return of
                      Return.NonTail {cont, handler} =>
                        let
                          val newHandler = case handler of
                                             Handler.Handle l => Handler.Handle(f(l))
                                           | _ => handler
                        in
                          Return.NonTail {cont = f(cont), handler = newHandler}
                        end
                    | _ => return
                in
                  Transfer.Call {args = args, func = func, return = newReturn}
                end
            | Transfer.Case {cases, default, test} =>
                let
                  val newCases = Cases.map(cases, f)
                  val newDefault = case default of
                                     NONE => default
                                   | SOME(l) => SOME(f(l))
                in
                  Transfer.Case {cases = newCases, default = newDefault, test = test}
                end
            | Transfer.Goto {args, dst} =>
                if Label.equals (dst, headerLabel) then
                  Transfer.Goto {args = vectorDrop(args, argi), dst = nextLabel}
                else
                  Transfer.Goto {args = args, dst = f(dst)}
            | Transfer.Runtime {args, prim, return} =>
                Transfer.Runtime {args = args, prim = prim, return = f(return)}
            | _ => transfer
      in
        Block.T {args = newArgs,
                 label = label,
                 statements = newStmts,
                 transfer = newTransfer}
      end)
  in
    Vector.rev fixedBlocks
  end

(* Unroll a loop. The header should ALWAYS be the first element in the returned list. *)
fun unrollLoop (oldHeader, argi, loopBlocks, argLabels, blockInfo, setBlockInfo) =
  let
    val oldHeaderLabel = Block.label oldHeader
    val oldHeaderArgs = Block.args oldHeader
    val oldTransfer = Block.transfer oldHeader
    val loopLabels = Vector.map (loopBlocks, Block.label)
  in
    case argLabels of
      [] =>
        let
          val (exitLabel, _, _) = loopExit (loopLabels, oldTransfer)
          val newTransfer = Transfer.Goto {args = Vector.new0 (),
                                           dst = exitLabel}
        in
          [Block.T {args = vectorDrop(oldHeaderArgs, argi),
                    label = Label.newNoname (),
                    statements = Vector.new0 (),
                    transfer = newTransfer}]
        end
    | hd::tl =>
        let
          val res = unrollLoop (oldHeader, argi, loopBlocks, tl, blockInfo, setBlockInfo)
          val nextBlockLabel = Block.label (List.first res)
          val newLoop = copyLoop(loopBlocks, nextBlockLabel, oldHeaderLabel,
                                 argi, hd, blockInfo, setBlockInfo)
        in
          (Vector.toList newLoop) @ res
        end
  end

(* Attempt to optimize a single loop. Returns a list of blocks to add to the program
   and a list of blocks to remove from the program. *)
fun optimizeLoop(allBlocks, headerNodes, loopNodes,
                 labelNode, nodeBlock, loadGlobal, depth) =
   let
      val headers = Vector.map (headerNodes, nodeBlock)
      val loopBlocks = Vector.map (loopNodes, nodeBlock)
      val loopBlockNames = Vector.map (loopBlocks, Block.label)
      val optOpt =
            findOpportunity(allBlocks, loopBlocks, headers, loadGlobal, depth)
      val {get = blockInfo: Label.t -> BlockInfo,
         set = setBlockInfo: Label.t * BlockInfo -> unit, destroy} =
            Property.destGetSet(Label.plist,
                                Property.initRaise("blockInfo", Label.layout))
   in
      case optOpt of
        NONE => ([], [])
      | SOME (argi, loop) =>
          if Loop.isInfiniteLoop loop then
            (logs "Can't unroll: infinite loop" ;
             logs (concat["Index: ", Int.toString argi, Loop.toString loop]) ;
             ([], []))
          else 
            let
              val () = inc(optCount)
              val oldHeader = Vector.sub (headers, 0)
              val oldArgs = Block.args oldHeader
              val (_, oldType) = Vector.sub (oldArgs, argi)
              val argSize = case Type.dest oldType of
                              Type.Word wsize => wsize
                            | _ => raise Fail "Argument is not of type word"
              val () = logs(concat["Index: ", Int.toString argi,
                                   Loop.toString loop])
              val newEntry = Label.newNoname()
              val (newHeader, argLabels) =
                makeHeader (oldHeader, argi, Loop.makeConstants (loop, argSize), newEntry)
              (* For each induction variable value, copy the loop's body *)
              val newBlocks = unrollLoop (oldHeader, argi, loopBlocks, argLabels,
                                          blockInfo, setBlockInfo)
              (* Fix the first entry's label *)
              val firstBlock = List.first newBlocks
              val args' = Block.args firstBlock
              val statements' = Block.statements firstBlock
              val transfer' = Block.transfer firstBlock
              val newHead = Block.T {args = args',
                                     label = newEntry,
                                     statements = statements',
                                     transfer = transfer'}
              val newBlocks' = newHeader::(newHead::(listPop newBlocks))
              val () = destroy()
              val () = logs "Adding blocks"
              val () = List.foreach (newBlocks', fn b => logl (Block.layout b))
            in
              (newBlocks', (Vector.toList loopBlockNames))
            end
   end

(* Traverse sub-forests until the innermost loop is found. *)
fun traverseSubForest ({loops, notInLoop},
                       allBlocks,
                       enclosingHeaders,
                       labelNode, nodeBlock, loadGlobal, depth) =
   if (Vector.length loops) = 0 then
      optimizeLoop(allBlocks, enclosingHeaders, notInLoop,
                   labelNode, nodeBlock, loadGlobal, depth)
   else
      Vector.fold(loops, ([], []), fn (loop, (new, remove)) =>
         let
            val (nBlocks, rBlocks) =
               traverseLoop(loop, allBlocks, labelNode, nodeBlock, loadGlobal, depth + 1)
         in
            ((new @ nBlocks), (remove @ rBlocks))
         end)

(* Traverse loops in the loop forest. *)
and traverseLoop ({headers, child},
                  allBlocks,
                  labelNode, nodeBlock, loadGlobal, depth) =
      traverseSubForest ((Forest.dest child), allBlocks,
                         headers, labelNode, nodeBlock, loadGlobal, depth + 1)

(* Traverse the top-level loop forest. *)
fun traverseForest ({loops, ...}, allBlocks, labelNode, nodeBlock, loadGlobal) =
  let
    val () = logs (concat[(Int.toString (Vector.length loops)), " total loops"])
    (* Gather the blocks to add/remove *)
    val (newBlocks, blocksToRemove) =
      Vector.fold(loops, ([], []), fn (loop, (new, remove)) =>
        let
          val (nBlocks, rBlocks) =
            traverseLoop(loop, allBlocks, labelNode, nodeBlock, loadGlobal, 1)
        in
          ((new @ nBlocks), (remove @ rBlocks))
        end)
    val keep: Block.t -> bool =
      (fn b => not (List.contains(blocksToRemove, (Block.label b), Label.equals)))
    val reducedBlocks = Vector.keepAll(allBlocks, keep)
  in
    (Vector.toList reducedBlocks) @ newBlocks
  end

(* Performs the optimization on the body of a single function. *)
fun optimizeFunction loadGlobal function =
   let
      val {graph, labelNode, nodeBlock} = Function.controlFlow function
      val {args, blocks, mayInline, name, raises, returns, start} = Function.dest function
      val () = logl (Func.layout name)
      val root = labelNode start
      val forest = Graph.loopForestSteensgaard(graph, {root = root})
      val newBlocks = traverseForest((Forest.dest forest),
                                     blocks, labelNode, nodeBlock, loadGlobal)
   in
      Function.new {args = args,
                    blocks = Vector.fromList(newBlocks),
                    mayInline = mayInline,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

(* Entry point. *)
fun transform (Program.T {datatypes, globals, functions, main}) =
   let
      fun loadGlobal (var : Var.t): IntInf.t option =
         let
            fun matchGlobal v g =
               case Statement.var g of
                 NONE => false
               | SOME (v') => Var.equals (v, v')
         in
            case Vector.peek (globals, matchGlobal var) of
              NONE => NONE
            | SOME (stmt) =>
               (case Statement.exp stmt of
                  Exp.Const c =>
                     (case c of
                        Const.Word w => SOME(WordX.toIntInf w)
                      | _ => NONE)
                | _ => NONE)
         end
      val () = optCount := 0
      val () = multiHeaders := 0
      val () = varEntryArg := 0
      val () = multiTransfer := 0
      val () = nonGoto := 0
      val () = ccTransfer := 0
      val () = ccBound := 0
      val () = infinite := 0
      val () = logs "Unrolling loops\n"
      val optimizedFunctions = List.map (functions, optimizeFunction loadGlobal)
      val restore = restoreFunction {globals = globals}
      val () = logs "Performing SSA restore"
      val cleanedFunctions = List.map (optimizedFunctions, restore)
      val shrink = shrinkFunction {globals = globals}
      val () = logs "Performing shrink"
      val shrunkFunctions = List.map (cleanedFunctions, shrink)
      val () = logs (concat[Int.toString(!optCount), " loops optimized"])
      val () = logs (concat[Int.toString(!multiHeaders), " loops had multiple headers"])
      val () = logs (concat[Int.toString(!varEntryArg),
                                         " loops had variable entry values"])
      val () = logs (concat[Int.toString(!multiTransfer),
                                         " loops had multiple transfers to the header"])
      val () = logs (concat[Int.toString(!nonGoto),
                                         " loops had non-goto transfers to the header"])
      val () = logs (concat[Int.toString(!ccTransfer),
                                         " loops had non-computable steps"])
      val () = logs (concat[Int.toString(!ccBound),
                                         " loops had non-computable bounds"])
      val () = logs (concat[Int.toString(!infinite),
                                         " infinite loops"])
      val () = logs "Done."
   in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = shrunkFunctions,
                 main = main}
   end

end
