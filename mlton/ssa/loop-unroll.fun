(* Copyright (C) 2016 Matthew Surawski.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Reduces or eliminates the iteration count of loops by duplicating
 * the loop body.
 *)
functor LoopUnroll (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer Prim

structure Graph = DirectedGraph
local
  open Graph
in
  structure Forest = LoopForest
end

fun ++ (v: int ref): unit =
  v := (!v) + 1


structure Histogram =
  struct
    type t = (IntInf.t * int ref) HashSet.t

    fun inc (set: t, key: IntInf.t): unit =
      let
        val _ = HashSet.insertIfNew (set, IntInf.hash key,
                                     (fn (k, _) => k = key),
                                     (fn () => (key, ref 1)),
                                     (fn (_, r) => ++r))
      in
        ()
      end

    fun new (): t =
      HashSet.new {hash = fn (k, _) => IntInf.hash k}

    fun toList (set: t): (IntInf.t * int ref) list =
      HashSet.toList set

    fun toString (set: t) : string =
      let
        val eles = toList set
      in
        List.fold (eles, "", fn ((k, r), s) => concat[s,
                                                      IntInf.toString k, ": ",
                                                      Int.toString (!r), "\n"])
      end
  end

val loopCount = ref 0
val optCount = ref 0
val total = ref 0
val partial = ref 0
val multiHeaders = ref 0
val varEntryArg = ref 0
val variantTransfer = ref 0
val unsupported = ref 0
val ccTransfer = ref 0
val varBound = ref 0
val infinite = ref 0
val boundDom = ref 0
val histogram = ref (Histogram.new ())

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
          if invert then
            (if start = b then
              false
            else if start < b andalso step > 0 then
              not (((b - start) mod step) = 0)
            else if start > b andalso step < 0 then
              not (((start - b) mod (~step)) = 0)
            else
              true)
          else
            step = 0
      | Lt b =>
        if invert then
          start >= b andalso step >= 0
        else
          start < b andalso step <= 0
      | Gt b =>
        if invert then
          start <= b andalso step <= 0
        else
          start > b andalso step >= 0


    fun iters (start: IntInf.t, step: IntInf.t, max: IntInf.t): IntInf.t =
      let
        val range = max - start
        val iters = range div step
        val adds = range mod step
      in
        if step > range then
          1
        else
          iters + adds
      end

    (* Assumes isInfiniteLoop is false, otherwise the result is undefined. *)
    fun iterCount (T {start, step, bound, invert}): IntInf.t =
      case bound of
        Eq b =>
          if invert then
            (b - start) div step
          else
            1
      | Lt b =>
        (case (start >= b, invert) of
          (true, false) => 0
        | (true, true) => iters (b - 1, ~step, start)
        | (false, true) => 0
        | (false, false) => iters (start, step, b))          
      | Gt b =>
        (case (start <= b, invert) of
          (true, false) => 0
        | (true, true) => iters (start, step, b + 1)
        | (false, true) => 0
        | (false, false) => iters (b, ~step, start))

    fun makeConstStmt (v: IntInf.t, wsize: WordSize.t): Var.t * Statement.t =
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

    fun makeVarStmt (v: IntInf.t, wsize: WordSize.t, var: Var.t)
                    : Var.t * Statement.t list =
      let
        val (cVar, cStmt) = makeConstStmt (v, wsize)
        val newExp = Exp.PrimApp {args = Vector.new2 (var, cVar),
                                  prim = Prim.wordAdd wsize,
                                  targs = Vector.new0 ()}
        val newType = Type.word wsize
        val newVar = Var.newNoname()
        val newStatement = Statement.T {exp = newExp,
                                        ty = newType,
                                        var = SOME(newVar)}
      in
        (newVar, [cStmt, newStatement])
      end

    (* Given:
        - a loop
        - a word size
       Returns:
        A variable and statement for the constant value after the loops final
        iteration. This value will make the loop exit.
      Assumes isInfiniteLoop is false, otherwise the result is undefined.
    *)
    fun makeLastConstant (T {start, step, bound, invert},
                          wsize: WordSize.t)
                          : Var.t list * Statement.t list =
      let
        val ic = iterCount (T {start = start,
                               step = step,
                               bound = bound,
                               invert = invert})
        val last = start + (step * ic)
        val (newVar, newStatement) = makeConstStmt(last, wsize)
      in
        ([newVar], [newStatement])
      end

    (* Given:
        - a loop
        - a word size
        - an iteration limit
      Returns:
        A pair of variables and statements for those variables
        for each iteration of the loop.
        This should go 1 step beyond the end of the loop.
      Assumes isInfiniteLoop is false, otherwise this will run forever. *)
    fun makeConstants (T {start, step, bound, invert},
                          wsize: WordSize.t,
                          limit: IntInf.t)
                         : Var.t list * Statement.t list =
      case bound of
        Eq b =>
          if (start = b) <> invert andalso limit > 0 then
            let
              val (newVar, newStatement) = makeConstStmt(start, wsize)
              val nextIter = T {start = start + step,
                                step = step,
                                bound = bound,
                                invert = invert}
              val (rVars, rStmts) = makeConstants (nextIter, wsize, limit - 1)
            in
              (newVar::rVars, newStatement::rStmts)
            end
          else if limit > 0 then
            let
              val (newVar, newStatement) = makeConstStmt(start, wsize)
            in
              ([newVar], [newStatement])
            end
          else
            ([], [])
      | Lt b =>
          if (start < b) <> invert andalso limit > 0 then
            let
              val (newVar, newStatement) = makeConstStmt(start, wsize)
              val nextIter = T {start = start + step,
                                step = step,
                                bound = bound,
                                invert = invert}
              val (rVars, rStmts) = makeConstants (nextIter, wsize, limit - 1)
            in
              (newVar::rVars, newStatement::rStmts)
            end
          else if limit > 0 then
            let
              val (newVar, newStatement) = makeConstStmt(start, wsize)
            in
              ([newVar], [newStatement])
            end
          else
            ([], [])
      | Gt b =>
          if (start > b) <> invert andalso limit > 0 then
            let
              val (newVar, newStatement) = makeConstStmt(start, wsize)
              val nextIter = T {start = start + step,
                                step = step,
                                bound = bound,
                                invert = invert}
              val (rVars, rStmts) = makeConstants (nextIter, wsize, limit - 1)
            in
              (newVar::rVars, newStatement::rStmts)
            end
          else if limit > 0 then
            let
              val (newVar, newStatement) = makeConstStmt(start, wsize)
            in
              ([newVar], [newStatement])
            end
          else
            ([], [])

    fun makeStepsRec (step: Step, wsize: WordSize.t,
                      var: Var.t, times: IntInf.t,
                      vList: Var.t list, sList: Statement.t list)
                      : Var.t list * Statement.t list =
      if times > 0 then
        let
          val stepAdd = step * (times - 1)
          val (newVar, newStatements) = makeVarStmt(stepAdd, wsize, var)
        in
          makeStepsRec (step, wsize, var, times - 1,
                        newVar::vList, newStatements @ sList)
        end
      else (vList, sList)

    fun makeSteps (T {step, ...},
                   wsize: WordSize.t,
                   var: Var.t,
                   times: IntInf.t)
                  : Var.t list * Statement.t list =
      makeStepsRec (step, wsize, var, times, [], [])

  end

fun logli (l: Layout.t, i: int): unit =
   Control.diagnostics
   (fn display =>
      display(Layout.indent(l, i * 2)))

fun logsi (s: string, i: int): unit =
   logli((Layout.str s), i)

fun logs (s: string): unit =
   logsi(s, 0)

fun logstat (x: int ref, s: string): unit =
  logs (concat[Int.toString(!x), " ", s])

fun listPop lst =
  case lst of
    [] => []
  | _::tl => tl

(* If a block was renamed, return the new name.
   Otherwise return the old name. *)
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
fun varConst (args, loadVar, signed): (Var.t * IntInf.int * bool) option =
   let
      val a1 = Vector.sub (args, 0)
      val a2 = Vector.sub (args, 1)
      val a1v = loadVar(a1, signed)
      val a2v = loadVar(a2, signed)
   in
      case (a1v, a2v) of
        (SOME x, NONE) => SOME (a2, x, false)
      | (NONE, SOME x) => SOME (a1, x, true)
      | _ => NONE
   end

(* Given:
    - an argument vector with two arguments
    - a primative operaton that is an addition or subtraction of a const value
    - a function from variables to their constant values
   Returns:
    -  The non-const variable and the constant value in terms of addition *)
fun checkPrim (args, prim, loadVar) =
  case Prim.name prim of
    Name.Word_add _ =>
      (case varConst(args, loadVar, false) of
        SOME(nextVar, x, _) => SOME (nextVar, x)
      | NONE => NONE)
  | Name.Word_addCheck (_, {signed}) =>
      (case varConst(args, loadVar, signed) of
        SOME(nextVar, x, _) => SOME(nextVar, x)
      | NONE => NONE)
  | Name.Word_sub _ =>
      (case varConst(args, loadVar, false) of
        SOME(nextVar, x, _) => SOME (nextVar, ~x)
      | NONE => NONE)
  | Name.Word_subCheck (_, {signed}) =>
      (case varConst(args, loadVar, signed) of
        SOME(nextVar, x, _) => SOME (nextVar, ~x)
      | NONE => NONE)
  | _ => NONE

(* Given:
    - a variable in the loop
    - another variable in the loop
    - the loop body
    - a function from variables to their constant values
    - a starting value, if the transfer to the header is an arith transfer
   Returns:
    - Some x such that the value of origVar in loop iteration i+1 is equal to
      (the value of origVar in iteration i) + x,
      or None if the step couldn't be computed *)
fun varChain (origVar, endVar, blocks, loadVar, total) =
   case Var.equals (origVar, endVar) of
     true => SOME (total)
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
                        checkPrim (args, prim, loadVar)
                      | _ => NONE))
             val label = Block.label b
             val blockArgs = Block.args b
             (* If we found the assignment or the block isn't unary,
                skip this step *)
             val arithTransfers =
              if ((Vector.length assignments) > 0) orelse
                 ((Vector.length blockArgs) <> 1)
              then
                Vector.new0 ()
              else
                let
                  val (blockArg, _) = Vector.sub (blockArgs, 0)
                  val blockEntrys = Vector.keepAllMap (blocks, fn b' =>
                    case Block.transfer b' of
                      Transfer.Arith {args, prim, success, ...} =>
                        if Label.equals (label, success) then
                           SOME(checkPrim(args, prim, loadVar))
                        else NONE
                    | Transfer.Call {return, ...} =>
                        (case return of
                           Return.NonTail {cont, ...} =>
                              if Label.equals (label, cont) then
                                 SOME(NONE)
                              else NONE
                         | _ => NONE)
                    | Transfer.Case {cases, ...} =>
                        (case cases of
                           Cases.Con v =>
                              if Vector.exists (v, fn (_, lbl) =>
                                 Label.equals (label, lbl)) then
                                   SOME(NONE)
                              else
                                 NONE
                         | Cases.Word (_, v) =>
                              if Vector.exists (v, fn (_, lbl) =>
                                 Label.equals (label, lbl)) then
                                   SOME(NONE)
                              else NONE)
                    | Transfer.Goto {args, dst} =>
                        if Label.equals (label, dst) then
                          SOME(SOME(Vector.sub (args, 0), 0))
                        else NONE
                    | _ => NONE)
                in
                  if Var.equals (endVar, blockArg) then
                    blockEntrys
                  else
                    Vector.new0 ()
                end  
             val assignments' =
              if Vector.length (arithTransfers) > 0 then
                case (Vector.fold (arithTransfers,
                                Vector.sub (arithTransfers, 0),
                                fn (trans, trans') =>
                                  case (trans, trans') of
                                    (SOME(a1, v1), SOME(a2, v2)) =>
                                      if Var.equals (a1, a2) andalso
                                         v1 = v2 then
                                        trans
                                      else
                                        NONE
                                  | _ => NONE)) of
                  SOME(a, v) => Vector.new1 (a, v)
                | NONE => assignments
              else
                assignments
          in
             case Vector.length assignments' of
                0 => NONE
              | 1 => SOME (Vector.sub (assignments', 0))
              | _ => raise Fail "Multiple assignments in SSA form!"
          end)
      in
         case endVarAssign of
           NONE => NONE
         | SOME (nextVar, x) =>
            varChain(origVar, nextVar, blocks, loadVar, x + total)
      end

(* Given:
    - a list of loop body labels
    - a transfer on a boolean value where one branch exits the loop
      and the other continues
   Returns:
    - the label that exits the loop
    - the label that continues the loop
    - true if the continue branch is the true branch
 *)
fun loopExit (loopLabels: Label.t vector, transfer: Transfer.t)
             : (Label.t * Label.t * bool) =
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
              | _ => raise Fail "This should be a con"
          in
            if Vector.contains (loopLabels, defaultLabel, Label.equals) then
              (caseLabel,
               defaultLabel,
               Con.equals (Con.fromBool false, caseCon))
            else
              (defaultLabel,
               caseLabel,
               Con.equals (Con.fromBool true, caseCon))
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
            | _ => raise Fail "This should be a con"))
          
  | _ => raise Fail "This should be a case statement"

fun isLoopBranch (loopLabels, cases, default) =
  case default of
    SOME (defaultLabel) =>
      (case cases of
        Cases.Con v =>
          if (Vector.length v) = 1 then
            let
              val (_, caseLabel) = Vector.sub (v, 0)
              val defaultInLoop =
                Vector.contains (loopLabels, defaultLabel, Label.equals)
              val caseInLoop =
                Vector.contains (loopLabels, caseLabel, Label.equals)
            in
              defaultInLoop <> caseInLoop 
            end
          else
            false
      | _ => false)
  | NONE =>
    (case cases of
      Cases.Con v =>
        if (Vector.length v) = 2 then
          let
            val (_, c1) = Vector.sub (v, 0)
            val (_, c2) = Vector.sub (v, 1)
            val c1il = Vector.contains (loopLabels, c1, Label.equals)
            val c2il = Vector.contains (loopLabels, c2, Label.equals)
          in
            c1il <> c2il
          end
        else
          false
    | _ => false)

fun transfersToHeader (headerLabel, block) =
  case Block.transfer block of
    Transfer.Arith {success, ...} =>
      Label.equals (headerLabel, success)
  | Transfer.Call {return, ...} =>
      (case return of
        Return.NonTail {handler, ...} =>
          (case handler of
            Handler.Handle l => Label.equals (headerLabel, l)
          | _ => false)
      | _ => false)
  | Transfer.Case {cases, ...} =>
      (* We don't have to check default because we know the header isn't nullary *)
      (case cases of
        Cases.Con v =>
          Vector.exists (v, (fn (_, lbl) => Label.equals (headerLabel, lbl)))
      | Cases.Word (_, v) =>
          Vector.exists (v, (fn (_, lbl) => Label.equals (headerLabel, lbl))))
  | Transfer.Goto {dst, ...} =>
      Label.equals (headerLabel, dst)
  | Transfer.Runtime {return, ...} =>
      Label.equals(headerLabel, return)
  | _ => false

(* Given:
    - a loop phi variable
    - that variables index in the loop header's arguments
    - that variables constant entry value (if it has one)
    - the loop header block
    - the loop body block
    - a function from variables to their constant values
   Returns:
    - a Loop structure for unrolling that phi var, if one exists *)
fun checkArg ((argVar, _), argIndex, entryArg, header, loopBody,
              loadVar: Var.t * bool -> IntInf.t option, domInfo, depth) =
   case entryArg of
      NONE => (logsi ("Can't unroll: entry arg not constant", depth) ;
               ++varEntryArg ;
               NONE)
   | SOME (entryX, entryXSigned) =>
      let
         val headerLabel = Block.label header
         val unsupportedTransfer = ref false

         (* For every transfer to the start of the loop
            get the variable at argIndex *)
         val loopVars = Vector.keepAllMap (loopBody, fn block => 
            case Block.transfer block of
               Transfer.Arith {args, prim, success, ...} =>
                  if Label.equals (headerLabel, success) then
                     case checkPrim (args, prim, loadVar) of
                       NONE => (unsupportedTransfer := true ; NONE)
                     | SOME (arg, x) => SOME (arg, x)
                  else NONE
             | Transfer.Call {return, ...} =>
                  (case return of
                     Return.NonTail {cont, ...} =>
                        if Label.equals (headerLabel, cont) then
                           (unsupportedTransfer := true ; NONE)
                        else NONE
                   | _ => NONE)
             | Transfer.Case {cases, ...} =>
                 (case cases of
                    Cases.Con v =>
                     if Vector.exists(v, fn (_, lbl) =>
                                           Label.equals (headerLabel, lbl)) then
                        (unsupportedTransfer := true ; NONE)
                     else NONE
                  | Cases.Word (_, v) =>
                     if Vector.exists(v, fn (_, lbl) =>
                                           Label.equals (headerLabel, lbl)) then
                        (unsupportedTransfer := true ; NONE)
                     else NONE)
             | Transfer.Goto {args, dst} =>
                  if Label.equals (headerLabel, dst) then
                     SOME (Vector.sub (args, argIndex), 0)
                  else NONE
            | _ => NONE)
      in
         if (Vector.length loopVars) > 1
         andalso not (Vector.forall
                      (loopVars, fn (arg, x) =>
                        let
                          val (arg0, x0) = Vector.sub (loopVars, 0)
                        in
                          Var.equals (arg0, arg) andalso (x0 = x)
                        end))
         then
            (logsi ("Can't unroll: variant transfer to head of loop", depth) ;
             ++variantTransfer ;
             NONE)
         else if (!unsupportedTransfer) then
            (logsi ("Can't unroll: unsupported transfer to head of loop",
                    depth) ;
             ++unsupported ;
             NONE)
         else
            let
               val (loopVar, x) = Vector.sub (loopVars, 0)
            in
               case varChain (argVar, loopVar, loopBody, loadVar, x) of
                 NONE => (logsi ("Can't unroll: can't compute transfer",
                                 depth) ; 
                          ++ccTransfer ;
                          NONE)
               | SOME (step) =>
                  let
                    fun ltOrGt (vc, signed) =
                      case vc of
                        NONE => NONE
                      | SOME (_, c, b) =>
                          if b then
                            SOME(Loop.Lt (c), signed)
                          else
                            SOME(Loop.Gt (c), signed)

                    fun eq (vc, signed) =
                      case vc of
                        NONE => NONE
                      | SOME (_, c, _) => SOME(Loop.Eq (c), signed)
                    val loopLabels = Vector.map (loopBody, Block.label)
                    val transferVarBlock = Vector.peekMap (loopBody, (fn b =>
                      let
                        val transferVar =
                          case Block.transfer b of
                            Transfer.Case {cases, default, test} =>
                              if isLoopBranch (loopLabels, cases, default) then
                                SOME(test)
                              else NONE
                          | _ => NONE
                        val loopBound =
                          case (transferVar) of
                            NONE => NONE
                          | SOME (tVar) =>
                              Vector.peekMap (Block.statements b,
                                (fn s => case Statement.var s of
                                  NONE => NONE
                                | SOME (sVar) =>
                                  if Var.equals (tVar, sVar) then
                                    case Statement.exp s of
                                      PrimApp {args, prim, ...} =>
                                        if not (Vector.contains
                                                (args, argVar, Var.equals))
                                        then
                                           NONE
                                        else
                                          (case Prim.name prim of
                                            Name.Word_lt (_, {signed}) =>
                                              ltOrGt
                                                (varConst (args,
                                                           loadVar,
                                                           signed),
                                                 signed)
                                          | Name.Word_equal _ =>
                                              eq
                                                (varConst (args,
                                                           loadVar,
                                                           false),
                                                 false)
                                          | _ => NONE)
                                    | _ => NONE
                                  else NONE))
                      in
                        case loopBound of
                          NONE => NONE
                        | SOME (bound, signed) =>
                            SOME(bound, b, signed)
                      end))
                  in
                    case transferVarBlock of
                      NONE =>
                        (logsi ("Can't unroll: can't determine bound", depth) ;
                         ++varBound ;
                         NONE)
                    | SOME(bound, block, signed) =>
                        let
                          val headerTransferBlocks =
                            Vector.keepAll(loopBody, (fn b =>
                              transfersToHeader (headerLabel, b)))
                          val boundDominates = Vector.forall (headerTransferBlocks,
                            (fn b => List.exists ((domInfo (Block.label b)),
                              (fn l => Label.equals
                                ((Block.label block), l)))))
                          val loopLabels = Vector.map (loopBody, Block.label)
                          val (_, _, contIsTrue) =
                                loopExit (loopLabels, Block.transfer block)
                          val entryVal = if signed then entryXSigned
                                         else entryX
                        in
                          if boundDominates then
                            SOME (argIndex,
                                  block,
                                  Loop.T {start = entryVal,
                                          step = step,
                                          bound = bound,
                                          invert = not contIsTrue})
                          else
                            (logsi ("Can't unroll: bound doesn't dominate", depth) ;
                             ++boundDom ;
                             NONE)
                        end
                  end
            end
      end
(* Check all of a loop's entry point arguments to see if a constant value.
   Returns a list of int options where SOME(x) is always x for each entry. *)
fun findConstantStart (entryArgs:
                                 (((IntInf.t * IntInf.t) option) vector) vector)
                      : ((IntInf.t * IntInf.t) option) vector =
  if (Vector.length entryArgs) > 0 then
    Vector.rev (Vector.fold (entryArgs, Vector.sub (entryArgs, 0),
      fn (v1, v2) => Vector.fromList (
        Vector.fold2 (v1, v2, [], fn (a1, a2, lst) =>
          case (a1, a2) of
            (SOME(x1, x1'), SOME(x2, _)) =>
              if x1 = x2 then SOME(x1, x1')::lst
              else NONE::lst
          | _ => NONE::lst))))
  else Vector.new0 ()

(* Look for any optimization opportunities in the loop. *)
fun findOpportunity(functionBody: Block.t vector,
                    loopBody: Block.t vector,
                    loopHeaders: Block.t vector,
                    loadGlobal: Var.t * bool -> IntInf.t option,
                    domInfo: Label.t -> Label.t list,
                    depth: int):
                    (int * Block.t * Loop.t) option =
   if (Vector.length loopHeaders) = 1 then
      let
         val header = Vector.sub (loopHeaders, 0)
         val headerArgs = Block.args header
         val headerLabel = Block.label header
         val () = logsi (concat["Evaluating loop with header: ",
                                Label.toString headerLabel], depth - 1)
         fun blockEquals (b1, b2) =
          Label.equals (Block.label b1, Block.label b2)
         val emptyArgs = SOME(Vector.new (Vector.length headerArgs, NONE))
         val entryArgs = Vector.keepAllMap(functionBody, fn block =>
                          if Vector.contains (loopBody, block, blockEquals) then
                            NONE
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
                                SOME(Vector.map (args, fn a =>
                                  case (loadGlobal(a, false),
                                        loadGlobal(a, true))
                                  of
                                    (NONE, NONE) => NONE
                                  | (SOME v1, SOME v2) => SOME (v1, v2)
                                  | _ => raise Fail "Impossible"))
                              else NONE
                          | _ => NONE)
         val () = logsi (concat["Loop has ",
                               Int.toString (Vector.length entryArgs),
                               " entry points"], depth - 1)
         val constantArgs = findConstantStart entryArgs
         val unrollableArgs =
          Vector.keepAllMapi
            (headerArgs, fn (i, arg) => (
               logsi (concat["Checking arg: ", Var.toString (#1 arg)], depth) ;
               checkArg (arg, i, Vector.sub (constantArgs, i),
                         header, loopBody, loadGlobal, domInfo, depth + 1)))
      in
        if (Vector.length unrollableArgs) > 0 then
          SOME(Vector.sub (unrollableArgs, 0))
        else NONE
      end
   else
      (logsi ("Can't optimize: loop has more than 1 header", depth) ;
       multiHeaders := (!multiHeaders) + 1 ;
       NONE)

fun makeHeader(oldHeader, (newVars, newStmts), newEntry) =
  let
    val oldArgs = Block.args oldHeader
    val newArgs = Vector.map (oldArgs, fn (arg, _) => arg)
    val newTransfer = Transfer.Goto {args = newArgs, dst = newEntry}
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
   Replace all instances of argi with argVar *)
fun copyLoop(blocks: Block.t vector,
             nextLabel: Label.t,
             headerLabel: Label.t,
             tBlock: Block.t,
             argi: int,
             argVar: Var.t,
             rewriteTransfer: bool,
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
    val fixedBlocks = Vector.map
      (newBlocks, fn Block.T {args, label, statements, transfer} =>
      let
        val f = fn l => fixLabel(blockInfo, l, labels)
        val isHeader = Label.equals (label, f(headerLabel))
        val (newArgs, unrolledArg) =
          if isHeader then
            (args, SOME(Vector.sub (args, argi)))
          else (args, NONE)
        val newStmts =
          if isHeader then
            case unrolledArg of
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
          if rewriteTransfer andalso
             Label.equals (label, f(Block.label tBlock))
          then
            let
              val (_, contLabel, _) = loopExit(labels, transfer)
            in
              Transfer.Goto {args = Vector.new0 (), dst = f(contLabel)}
            end
          else
            case transfer of
              Transfer.Arith {args, overflow, prim, success, ty} =>
                if Label.equals (success, headerLabel) then
                  Transfer.Arith {args = args,
                                  overflow = f(overflow),
                                  prim = prim,
                                  success = nextLabel,
                                  ty = ty}
                else
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
                          val newHandler =
                            case handler of
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
                  Transfer.Case {cases = newCases,
                                 default = newDefault,
                                 test = test}
                end
            | Transfer.Goto {args, dst} =>
                if Label.equals (dst, headerLabel) then
                  Transfer.Goto {args = args, dst = nextLabel}
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

(* Unroll a loop. The header should ALWAYS be the first element
   in the returned list. *)
fun unrollLoop (oldHeader, tBlock, argi, loopBlocks, argLabels,
                exit, rewriteTransfer, blockInfo, setBlockInfo) =
  let
    val oldHeaderLabel = Block.label oldHeader
  in
    case argLabels of
      [] => [exit]
    | hd::tl =>
        let
          val res = unrollLoop (oldHeader, tBlock, argi,
                                loopBlocks, tl, exit, rewriteTransfer,
                                blockInfo, setBlockInfo)
          val nextBlockLabel = Block.label (List.first res)
          val newLoop = copyLoop(loopBlocks, nextBlockLabel, oldHeaderLabel,
                                 tBlock, argi, hd, rewriteTransfer,
                                 blockInfo, setBlockInfo)
        in
          (Vector.toList newLoop) @ res
        end
  end

(* Given:
    - an itertion count
    - a loop body
   Returns (b, x, y, z) such that:
      if b is true
        - unroll the loop completely
        - x, y, and z are undefined.
      if b is false
        - x is the number of times to expand the loop body
        - y is the number of iterations to run the expanded body (must never be 0)
        - z is the number of times to peel the loop body
 *)
fun shouldOptimize (iterCount, loopBlocks, depth) =
  let
    val loopSize' = Block.sizeV (loopBlocks, {sizeExp = Exp.size, sizeTransfer = Transfer.size})
    val loopSize = IntInf.fromInt loopSize'
    val unrollLimit = IntInf.fromInt (!Control.loopUnrollLimit)
    val () = logsi ("iterations * loop size < unroll factor = can total unroll",
                    depth)
    val canTotalUnroll = (iterCount * loopSize) < unrollLimit
    val () = logsi (concat[IntInf.toString iterCount, " * ",
                           IntInf.toString loopSize, " < ",
                           IntInf.toString unrollLimit, " = ",
                           Bool.toString canTotalUnroll], depth)
  in
    if (iterCount = 1) orelse canTotalUnroll then
      (* Loop runs once or it's small enough to unroll *)
      (true, 0, 0, 0)
    else if loopSize >= unrollLimit then
      (* Loop is too big to unroll at all, peel off 1 iteration *)
      (false, 1, iterCount - 1, 1)
    else
      let
        val exBodySize = unrollLimit div loopSize
        val exIters = iterCount div exBodySize
        val leftovers = iterCount - (exIters * exBodySize)
      in
        if (exIters - 1) < 2 then
          (* If the unpeeled loop would run 1 or 0 times, just unroll the
             whole thing *)
          (true, 0, 0, 0)
        else
          if leftovers = 0 then
            (* If we don't get any unpeelings naturally, force one *)
            (false, exBodySize, exIters - 1, exBodySize)
          else
            (* Otherwise stick them on the front of the loop *)
            (false, exBodySize, exIters, leftovers)
      end
  end

fun expandLoop (oldHeader, loopBlocks, loop, tBlock, argi, argSize, oldArg,
                exBody, iterBody, exitLabel,  blockInfo, setBlockInfo) =
  let
    (* Make a new loop header with an additional arg *)
    val newLoopEntry = Label.newNoname()
    val (newLoopHeader, loopArgLabels) =
      makeHeader (oldHeader,
                  Loop.makeSteps (loop, argSize, oldArg, exBody),
                  newLoopEntry)
    val iterVar = Var.newNoname ()
    val newLoopHeaderArgs' = Vector.concat
                              [Block.args newLoopHeader,
                               Vector.new1 (iterVar, Type.word argSize)]
    val newLoopHeader' = 
      Block.T {args = newLoopHeaderArgs',
               label = Label.newNoname (),
               statements = Block.statements newLoopHeader,
               transfer = Block.transfer newLoopHeader}

    (* Make a new goto to the top of the loop increasing the iter by 1 *)
    val loopHeaderGoto =
      let
        val (newVar, newVarStmts) = Loop.makeVarStmt (1, argSize, iterVar)
        val nonIterArgs = Vector.map (Block.args oldHeader, fn (a, _) => a)
        val newArgs = Vector.concat [nonIterArgs, Vector.new1 (newVar)]
        val newTransfer = Transfer.Goto {args = newArgs,
                                         dst = Block.label newLoopHeader'}
      in
        Block.T {args = Vector.new0 (),
                label = Label.newNoname (),
                statements = Vector.fromList newVarStmts,
                transfer = newTransfer}
      end

    val newLoopExit =
      let
        val (newLimitVar, newLimitStmt) =
          Loop.makeConstStmt (iterBody - 1, argSize)
        val (newComp, newCompVar) =
          let
            val newVar = Var.newNoname ()
            val newTy = Type.datatypee Tycon.bool
            val newExp =
              PrimApp {args = Vector.new2 (iterVar, newLimitVar),
                       prim = Prim.wordLt (argSize, {signed = true}),
                       targs = Vector.new0 ()}
          in
            (Statement.T {exp = newExp,
                          ty = newTy,
                          var = SOME(newVar)},
             newVar)
          end
        val exitStatements = Vector.new2(newLimitStmt, newComp)
        val exitCases = Cases.Con (
                          Vector.new1 (Con.fromBool true,
                                       Block.label loopHeaderGoto))
        val exitTransfer = Transfer.Case {cases = exitCases,
                                          default = SOME(exitLabel),
                                          test = newCompVar}
      in
        Block.T {args = Block.args oldHeader,
                 label = Label.newNoname (),
                 statements = exitStatements,
                 transfer = exitTransfer}
      end

    (* Expand the loop exBody times. Rewrite the bound's transfer,
       because we know it will always be true and it won't be eliminated
       by shrink. *)
    val newLoopBlocks = unrollLoop (oldHeader, tBlock, argi,
                                loopBlocks, loopArgLabels, newLoopExit,
                                true, blockInfo, setBlockInfo)
    val firstLoopBlock = List.first newLoopBlocks
    val loopArgs' = Block.args firstLoopBlock
    val loopStatements' = Block.statements firstLoopBlock
    val loopTransfer' = Block.transfer firstLoopBlock
    val newLoopHead = Block.T {args = loopArgs',
                           label = newLoopEntry,
                           statements = loopStatements',
                           transfer = loopTransfer'}
    val newLoopBlocks' = newLoopHeader'::
                          (loopHeaderGoto::
                            (newLoopHead::
                              (listPop newLoopBlocks)))
  in
    newLoopBlocks'
  end

(* Attempt to optimize a single loop. Returns a list of blocks to add to the
   program and a list of blocks to remove from the program. *)
fun optimizeLoop(allBlocks, headerNodes, loopNodes,
                 nodeBlock, loadGlobal, domInfo, depth) =
   let
      val () = ++loopCount
      val headers = Vector.map (headerNodes, nodeBlock)
      val loopBlocks = Vector.map (loopNodes, nodeBlock)
      val loopBlockNames = Vector.map (loopBlocks, Block.label)
      val optOpt = findOpportunity (allBlocks, loopBlocks, headers,
                                    loadGlobal, domInfo, depth + 1)
      val {get = blockInfo: Label.t -> BlockInfo,
         set = setBlockInfo: Label.t * BlockInfo -> unit, destroy} =
            Property.destGetSet(Label.plist,
                                Property.initRaise("blockInfo", Label.layout))
   in
      case optOpt of
        NONE => ([], [])
      | SOME (argi, tBlock, loop) =>
          if Loop.isInfiniteLoop loop then
            (logsi ("Can't unroll: infinite loop", depth) ;
             ++infinite ;
             logsi (concat["Index: ", Int.toString argi, Loop.toString loop],
                    depth) ;
             ([], []))
          else
            let
              val () = ++optCount
              val oldHeader = Vector.sub (headers, 0)
              val oldArgs = Block.args oldHeader
              val (oldArg, oldType) = Vector.sub (oldArgs, argi)
              val () = logsi (concat["Can unroll loop on ",
                                     Var.toString oldArg], depth)
              val () = logsi (concat["Index: ", Int.toString argi,
                                   Loop.toString loop], depth)
              val iterCount = Loop.iterCount loop
              val () = logsi (concat["Loop will run ",
                                     IntInf.toString iterCount,
                                     " times"], depth)
              val () = logsi (concat["Transfer block is ",
                                      Label.toString (Block.label tBlock)],
                              depth)
              val () = Histogram.inc ((!histogram), iterCount)
              val (totalUnroll, exBody, iterBody, peel) =
                    shouldOptimize (iterCount, loopBlocks, depth + 1)
              val argSize = case Type.dest oldType of
                              Type.Word wsize => wsize
                            | _ => raise Fail "Argument is not of type word"
            in
              if totalUnroll then
                let
                  val () = ++total
                  val () = logsi ("Completely unrolling loop", depth)
                  val newEntry = Label.newNoname()
                  val (newHeader, argLabels) =
                    makeHeader (oldHeader,
                                Loop.makeConstants (loop, argSize, iterCount+1),
                                newEntry)
                  val exitBlock = Block.T {args = oldArgs,
                                           label = Label.newNoname (),
                                           statements = Vector.new0 (),
                                           transfer = Transfer.Bug}
                  (* For each induction variable value, copy the loop's body *)
                  val newBlocks = unrollLoop (oldHeader, tBlock, argi,
                                              loopBlocks, argLabels, exitBlock,
                                              false, blockInfo, setBlockInfo)
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
                in
                  (newBlocks', (Vector.toList loopBlockNames))
                end
              else
                let
                  val () = ++partial
                  val () = logsi ("Partially unrolling loop", depth)
                  val () = logsi (concat["Body expansion: ",
                                         IntInf.toString exBody,
                                         " Body iterations: ",
                                         IntInf.toString iterBody,
                                         " Peel iterations: ",
                                         IntInf.toString peel],
                                  depth)
                  val oldArgLabels = Vector.map (oldArgs, fn (a, _) => a)
                  (* Produce an exit loop iteration. *)
                  val exitEntry = Label.newNoname()
                  val (exitHeader, exitConsts) =
                    makeHeader (oldHeader,
                                Loop.makeLastConstant (loop, argSize),
                                exitEntry)
                  val exitHeader' =
                    Block.T {args = Block.args exitHeader,
                             label = Label.newNoname (),
                             statements = Block.statements exitHeader,
                             transfer = Block.transfer exitHeader}
                  val exitBlock = Block.T {args = oldArgs,
                                           label = Label.newNoname (),
                                           statements = Vector.new0 (),
                                           transfer = Transfer.Bug}
                  val exitBlocks = unrollLoop (oldHeader, tBlock, argi,
                                              loopBlocks, exitConsts, exitBlock,
                                              false, blockInfo, setBlockInfo)
                  val exitFirstBlock = List.first exitBlocks
                  val exitArgs = Block.args exitFirstBlock
                  val exitStatements = Block.statements exitFirstBlock
                  val exitTransfer = Block.transfer exitFirstBlock
                  val exitHead = Block.T {args = exitArgs,
                                         label = exitEntry,
                                         statements = exitStatements,
                                         transfer = exitTransfer}
                  val exitGotoLabel = Label.newNoname()
                  val exitGoto = Block.T {args = Vector.new0 (),
                                          label = exitGotoLabel,
                                          statements = Vector.new0 (),
                                          transfer =
                                            Transfer.Goto
                                              {args = oldArgLabels,
                                               dst = Block.label exitHeader'}}
                  val exitBlocks' = exitGoto::
                                    exitHeader'::
                                    (exitHead::(listPop exitBlocks))

                  (* Expand the loop *)
                  val exLoopBlocks = expandLoop (oldHeader, loopBlocks, loop,
                                                 tBlock, argi, argSize,
                                                  oldArg, exBody, iterBody,
                                                  exitGotoLabel,
                                                  blockInfo, setBlockInfo)
                  (* Make an entry to the expanded loop *)
                  val exLoopEntry =
                    let
                      val (zeroVar, zeroStmt) = Loop.makeConstStmt(0, argSize)
                      val exLoopHeader = Block.label (List.first exLoopBlocks)
                      val transferArgs =
                        Vector.concat [oldArgLabels, Vector.new1(zeroVar)]
                      val newTransfer = Transfer.Goto {args = transferArgs,
                                                       dst = exLoopHeader}
                    in
                      Block.T {args = oldArgs,
                               label = Label.newNoname(),
                               statements = Vector.new1 zeroStmt,
                               transfer = newTransfer}
                    end
                  (* Make a replacement loop entry *)
                  val newEntry = Label.newNoname()
                  val (newHeader, argLabels) =
                    makeHeader (oldHeader,
                                Loop.makeConstants (loop, argSize, peel),
                                newEntry)
                  (* For each induction variable value, copy the loop's body *)
                  val newBlocks = unrollLoop (oldHeader, tBlock, argi,
                                              loopBlocks, argLabels, exLoopEntry,
                                              false, blockInfo, setBlockInfo)
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
                in
                  (newBlocks' @ exLoopBlocks @ exitBlocks',
                   (Vector.toList loopBlockNames))
                end
            end
   end

(* Traverse sub-forests until the innermost loop is found. *)
fun traverseSubForest ({loops, notInLoop},
                       allBlocks,
                       enclosingHeaders,
                       labelNode, nodeBlock, loadGlobal, domInfo) =
   if (Vector.length loops) = 0 then
      optimizeLoop(allBlocks, enclosingHeaders, notInLoop,
                   nodeBlock, loadGlobal, domInfo, 1)
   else
      Vector.fold(loops, ([], []), fn (loop, (new, remove)) =>
         let
            val (nBlocks, rBlocks) =
               traverseLoop(loop, allBlocks, labelNode, nodeBlock, loadGlobal, domInfo)
         in
            ((new @ nBlocks), (remove @ rBlocks))
         end)

(* Traverse loops in the loop forest. *)
and traverseLoop ({headers, child},
                  allBlocks,
                  labelNode, nodeBlock, loadGlobal, domInfo) =
      traverseSubForest ((Forest.dest child), allBlocks,
                         headers, labelNode, nodeBlock, loadGlobal, domInfo)

(* Traverse the top-level loop forest. *)
fun traverseForest ({loops, notInLoop = _}, allBlocks, labelNode, nodeBlock, loadGlobal, domInfo) =
  let
    (* Gather the blocks to add/remove *)
    val (newBlocks, blocksToRemove) =
      Vector.fold(loops, ([], []), fn (loop, (new, remove)) =>
        let
          val (nBlocks, rBlocks) =
            traverseLoop(loop, allBlocks, labelNode, nodeBlock, loadGlobal, domInfo)
        in
          ((new @ nBlocks), (remove @ rBlocks))
        end)
    val keep: Block.t -> bool =
      (fn b => not (List.contains(blocksToRemove, (Block.label b), Label.equals)))
    val reducedBlocks = Vector.keepAll(allBlocks, keep)
  in
    (Vector.toList reducedBlocks) @ newBlocks
  end

fun setDoms tree =
  let
    val {get = domInfo: Label.t -> Label.t list,
         set = setDomInfo: Label.t * Label.t list -> unit, destroy} =
            Property.destGetSet(Label.plist,
                                Property.initRaise("domInfo", Label.layout))
    fun loop (tree, doms) =
      case tree of
        Tree.T (block, children) =>
          (setDomInfo (Block.label block, doms) ;
           Vector.foreach (children, fn tree => loop(tree,
                                                     (Block.label block)::doms)))
    val () = loop (tree, [])
  in
    (domInfo, destroy)
  end

(* Performs the optimization on the body of a single function. *)
fun optimizeFunction loadGlobal function =
   let
      val {graph, labelNode, nodeBlock} = Function.controlFlow function
      val {args, blocks, mayInline, name, raises, returns, start} =
        Function.dest function
      val fsize = Function.size (function, {sizeExp = Exp.size, sizeTransfer = Transfer.size})
      val () = logs (concat["Optimizing function: ", Func.toString name,
                            " of size ", Int.toString fsize])
      val root = labelNode start
      val forest = Graph.loopForestSteensgaard(graph, {root = root})
      val dtree = Function.dominatorTree function
      val (domInfo, destroy) = setDoms dtree
      val newBlocks = traverseForest((Forest.dest forest),
                                     blocks, labelNode, nodeBlock, loadGlobal, domInfo)
      val () = destroy()
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
      fun loadGlobal (var: Var.t, signed: bool): IntInf.t option =
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
                        Const.Word w =>
                          if signed then
                            SOME(WordX.toIntInfX w)
                          else
                            SOME(WordX.toIntInf w)
                      | _ => NONE)
                | _ => NONE)
         end
      val () = loopCount := 0
      val () = total := 0
      val () = partial := 0
      val () = optCount := 0
      val () = multiHeaders := 0
      val () = varEntryArg := 0
      val () = variantTransfer := 0
      val () = unsupported := 0
      val () = ccTransfer := 0
      val () = varBound := 0
      val () = infinite := 0
      val () = boundDom := 0
      val () = histogram := Histogram.new ()
      val () = logs (concat["Unrolling loops. Unrolling factor = ",
                    Int.toString (!Control.loopUnrollLimit)])
      val optimizedFunctions = List.map (functions, optimizeFunction loadGlobal)
      val restore = restoreFunction {globals = globals}
      val () = logs "Performing SSA restore"
      val cleanedFunctions = List.map (optimizedFunctions, restore)
      val shrink = shrinkFunction {globals = globals}
      val () = logs "Performing shrink"
      val shrunkFunctions = List.map (cleanedFunctions, shrink)
      val () = logstat (loopCount,
                        "total innermost loops")
      val () = logstat (optCount,
                        "loops optimized")
      val () = logstat (total,
                        "loops completely unrolled")
      val () = logstat (partial,
                        "loops partially unrolled")
      val () = logstat (multiHeaders,
                        "loops had multiple headers")
      val () = logstat (varEntryArg,
                        "variable entry values")
      val () = logstat (variantTransfer,
                        "loops had variant transfers to the header")
      val () = logstat (unsupported,
                        "loops had unsupported transfers to the header")
      val () = logstat (ccTransfer,
                        "loops had non-computable steps")
      val () = logstat (varBound,
                        "loops had variable bounds")
      val () = logstat (infinite,
                        "infinite loops")
      val () = logstat (boundDom,
                        "loops had non-dominating bounds")
      val () = logs ("Iterations: Occurences")
      val () = logs (Histogram.toString (!histogram))
      val () = logs "Done."
   in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = shrunkFunctions,
                 main = main}
   end

end
