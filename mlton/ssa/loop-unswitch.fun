(* Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Moves a conditional statement outside a loop by duplicating the loops body
 * under each branch of the conditional.
 *)
functor LoopUnswitch(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer

structure Graph = DirectedGraph
local
   open Graph
in
   structure Node = Node
   structure Forest = LoopForest
end

type BlockInfo = Label.t * (Var.t * Type.t) vector

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

fun loglli (ll: Label.t list, i: int): unit =
	logli (Layout.align (List.map (ll, Label.layout)), i)

fun loglvi (lv: Label.t vector, i: int): unit =
	loglli (Vector.toList(lv), i)

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

(* Copy an entire loop *)
fun copyLoop(blocks: Block.t vector,
             labels: Label.t vector,
             blockInfo: Label.t -> BlockInfo,
             setBlockInfo: Label.t * BlockInfo -> unit): Block.t vector =
  let
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
        val newTransfer =
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
          | Transfer.Goto {args, dst} => Transfer.Goto {args = args, dst = f(dst)}
          | Transfer.Runtime {args, prim, return} =>
              Transfer.Runtime {args = args, prim = prim, return = f(return)}
          | _ => transfer
      in
        Block.T {args = args,
                 label = label,
                 statements = statements,
                 transfer = newTransfer}
      end)
  in
    fixedBlocks
  end

fun blockVars (block: Block.t): Var.t list =
  let
    val args = Vector.fold ((Block.args block), [], (fn ((var, _), lst) => var::lst))
    val stmts = Vector.fold ((Block.statements block), [], (fn (stmt, lst) =>
      case Statement.var stmt of
        NONE => lst
      | SOME(v) => v::lst))
  in
    args @ stmts
  end

fun caseLabels (cases: Cases.t): Label.t vector =
  case cases of
    Cases.Con(c) => Vector.map(c, fn (_, label) => label)
  | Cases.Word(_, w) => Vector.map(w, fn (_, label) => label)

fun detectCases(block: Block.t, vars: Var.t list, labels: Label.t vector) =
  let
    val zeroStatements = Vector.length(Block.statements block) = 0
    val transfer = Block.transfer block
    val tdata =
      (case transfer of
        Case {cases, default, test} => SOME(test, cases, default)
      | _ => NONE)
  in
    case tdata of
      NONE => NONE
    | SOME(tvar, tcases, tdefault) =>
      (let
        val blockName = Block.label block
        val () = logs (concat ["Evaluating ", Label.toString(blockName)])
        val () = logl(Transfer.layout transfer)
        val tlabels = caseLabels tcases
        val varOutsideLoop = not (List.contains(vars, tvar, Var.equals))
        val labelsInsideLoop = Vector.forall(tlabels,
                                fn l => Vector.contains(labels, l, Label.equals))
        val noDefault = case tdefault of NONE => true | _ => false
        val canOptimize = varOutsideLoop (*andalso labelsInsideLoop  andalso
        			zeroStatements andalso noDefault*)
      in
        if canOptimize then
          let
            val () = logs("Can optimize!") 
          in
            SOME(blockName, tcases, tvar, tdefault)
          end
        else
          let
            val () = if not varOutsideLoop then
                        logs ("Can't optimize: condition not invariant")
                      else ()
          in
            NONE
          end
      end)
  end

(* Look for any optimization opportunities *)
fun findOpportunity(blocks: Block.t vector,
                    headers: Block.t vector,
                    depth: int)
                    : ((Label.t * Cases.t * Var.t * Label.t option) * Block.t) option =
  let
    val vars = Vector.fold (blocks, [], (fn (b, lst) => (blockVars b) @ lst))
    (*val _ = List.foreach (vars, (fn v => logli(Var.layout v, depth)))*)
    val blockNames = Vector.map (blocks, Block.label)
    val canOptimize = Vector.keepAllMap (blocks,
                                         fn b => detectCases (b, vars, blockNames))
  in
    if (Vector.length headers) = 1 then
      case Vector.length canOptimize of
        0 => NONE
      | _ => SOME(Vector.sub(canOptimize, 0), Vector.sub(headers, 0))
    else
    	let
    		val () = logsi ("Can't optimize, loop has more than 1 header", depth)
		in
			NONE
		end
  end

(* Copy a loop and set up the transfer *)
fun makeBranch (loopBody: Block.t vector,
                loopHeader: Block.t,
                branchLabel: Label.t,
                blockInfo: Label.t -> BlockInfo,
                setBlockInfo: Label.t * BlockInfo -> unit,
                labelNode: Label.t -> unit Node.t,
                nodeBlock: unit Node.t -> Block.t)
                : Block.t vector * Label.t =
   let
      (* Copy the loop body *)
      val loopBodyLabels = Vector.map (loopBody, Block.label)
      val newLoop = copyLoop(loopBody, loopBodyLabels,
                             blockInfo, setBlockInfo)
      (* Set up a goto for the loop *)
      val (newLoopHeaderLabel, _) = blockInfo(Block.label loopHeader)
      val newLoopArgs = Vector.map (Block.args loopHeader,
                                    fn (v, _) => v)
      val newLoopEntryTransfer = Transfer.Goto {args = newLoopArgs,
                                               dst = newLoopHeaderLabel}
      val newLoopEntryLabel = Label.newNoname()
      val newLoopEntryArgs =
         if Vector.contains (loopBodyLabels, branchLabel, Label.equals) then
            let
               val (_, args) = blockInfo(branchLabel)
            in
               args
            end
         else
            let
               val block = nodeBlock (labelNode branchLabel)
            in
               Block.args block
            end
      val newLoopEntry = Block.T {args = newLoopEntryArgs,
                                 label = newLoopEntryLabel,
                                 statements = Vector.new0(),
                                 transfer = newLoopEntryTransfer}

      (* Return the new loop, entrypoint, and entrypoint label *)
      val returnBlocks =
       Vector.concat [newLoop, (Vector.new1(newLoopEntry))]
   in
      (returnBlocks, newLoopEntryLabel)
   end

(* Attempt to optimize a single loop. Returns a list of blocks to add to the program
   and a list of blocks to remove from the program. *)
fun optimizeLoop(headerNodes, loopNodes, labelNode, nodeBlock, depth):
                                                        Block.t list * Label.t list =
  let
    val () = logsi ("At innermost loop", depth)
    val headers = Vector.map (headerNodes, nodeBlock)
    val blocks = Vector.map (loopNodes, nodeBlock)
    val blockNames = Vector.map (blocks, Block.label)
    val condLabelOpt = findOpportunity(blocks, headers, depth)
    val {get = blockInfo: Label.t -> BlockInfo,
         set = setBlockInfo: Label.t * BlockInfo -> unit, destroy} =
            Property.destGetSet(Label.plist,
                                Property.initRaise("blockInfo", Label.layout))
  in
    case condLabelOpt of
      NONE => ([], [])
    | SOME((label, cases, check, default), header) =>
        let
         val mkBranch = fn lbl => makeBranch(blocks, header, lbl, blockInfo,
                                             setBlockInfo, labelNode, nodeBlock)
         (* Copy the loop body for the default case if necessary *)
          val (newDefaultLoop, newDefault) =
            case default of
              NONE => ([], NONE)
            | SOME(defaultLabel) =>
                let
                  val (newLoop, newLoopEntryLabel) = mkBranch(defaultLabel)
                in
                  (Vector.toList newLoop, SOME(newLoopEntryLabel))
                end
          (* Copy the loop body for each case (except default) *)
          val (newLoops, newCases) =
            case cases of
              Cases.Con v =>
                let
                  val newLoopCases =
                    Vector.map(v,
                      fn (con, lbl) =>
                        let
                          val (newLoop, newLoopEntryLabel) = mkBranch(lbl)
                          val newCase = (con, newLoopEntryLabel)
                        in
                          (newLoop, newCase)
                        end)
                  val (newLoops, newCaseList) = Vector.unzip newLoopCases
                  val newCases = Cases.Con (newCaseList)
                in
                  (newLoops, newCases)
                end 
            | Cases.Word (size, v) =>
                let
                  val newLoopCases =
                    Vector.map(v,
                      fn (wrd, lbl) =>
                        let
                          val (newLoop, newLoopEntryLabel) = mkBranch(lbl)
                          val newCase = (wrd, newLoopEntryLabel)
                        in
                          (newLoop, newCase)
                        end)
                  val (newLoops, newCaseList) = Vector.unzip newLoopCases
                  val newCases = Cases.Word (size, newCaseList)
                in
                  (newLoops, newCases)
                end

         (* Produce a single list of new blocks *)
          val loopBlocks = Vector.fold(newLoops, newDefaultLoop, fn (loop, acc) =>
                              acc @ (Vector.toList loop))

          (* Produce a new entry block with the same label as the old loop header *)
          val newTransfer = Transfer.Case {cases = newCases,
                                           default = newDefault,
                                           test = check}
          val newEntry = Block.T {args = Block.args header,
                                  label = Block.label header,
                                  statements = Vector.new0(),
                                  transfer = newTransfer}
          val () = destroy()
        in
          (newEntry::loopBlocks, (Vector.toList blockNames))
        end
  end

(* Traverse sub-forests until the innermost loop is found *)
fun traverseSubForest ({loops, notInLoop},
                       enclosingHeaders,
                       labelNode, nodeBlock, depth): Block.t list * Label.t list =
  let
    val () = logsi ("Not in loop:", depth)
    val () = Vector.foreach (notInLoop, fn n =>
      let
        val block = nodeBlock n
        val blockName = Label.layout (Block.label block)
      in
        logli (blockName, depth)
      end)
      val () = ()
  in
    if (Vector.length loops) = 0 then
      optimizeLoop(enclosingHeaders, notInLoop, labelNode, nodeBlock, depth)
    else
      Vector.fold(loops, ([], []), fn (loop, (new, remove)) =>
        let
          val (nBlocks, rBlocks) = traverseLoop(loop, labelNode, nodeBlock, depth + 1)
        in
          ((new @ nBlocks), (remove @ rBlocks))
        end)
  end
(* Traverse loops in the loop forest *)
and traverseLoop ({headers, child},
                  labelNode, nodeBlock, depth): Block.t list * Label.t list =
   let
      val () = logsi ("Loop with headers:", depth)
      val nToL = Label.layout o Block.label o nodeBlock
      val () = logli ((Layout.align (Vector.toList(Vector.map(headers, nToL)))), depth)
   in
      traverseSubForest ((Forest.dest child), headers, labelNode, nodeBlock, depth + 1)
   end

(* Traverse the top-level loop forest *)
fun traverseForest ({loops, notInLoop}, allBlocks, labelNode, nodeBlock): Block.t list =
  let
    val () = logs (concat[(Int.toString (Vector.length loops)), " total loops"])
    val (newBlocks, blocksToRemove) =
      Vector.fold(loops, ([], []), fn (loop, (new, remove)) =>
        let
          val (nBlocks, rBlocks) = traverseLoop(loop, labelNode, nodeBlock, 1)
        in
          ((new @ nBlocks), (remove @ rBlocks))
        end)
    val () = logs("Removing blocks:")
    val btrl = List.map (blocksToRemove, Label.layout)
    val () = logl(Layout.align btrl)
    val () = logs("Adding blocks:")
    val btal = List.map (newBlocks, Label.layout o Block.label)
    val () = logl(Layout.align btal)
    val keep: Block.t -> bool =
      (fn b => not (List.contains(blocksToRemove, (Block.label b), Label.equals)))
    val reducedBlocks = Vector.keepAll(allBlocks, keep)
  in
    (Vector.toList reducedBlocks) @ newBlocks
  end

(* Performs the optimization on the body of a single function *)
fun optimizeFunction(function: Function.t): Function.t =
   let
      val {graph, labelNode, nodeBlock} = Function.controlFlow function
      val {args, blocks, mayInline, name, raises, returns, start} = Function.dest function
      val () = logl (Func.layout name)
      val root = labelNode start
      val forest = Graph.loopForestSteensgaard(graph, {root = root})
      val newBlocks = traverseForest((Forest.dest forest), blocks, labelNode, nodeBlock)
   in
      Function.new {args = args,
                    blocks = Vector.fromList(newBlocks),
                    mayInline = mayInline,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

(* Entry point *)
fun transform (Program.T {datatypes, globals, functions, main}) =
   let
      val () = logs "Unswitching loops"
      val optimizedFunctions = List.map (functions, optimizeFunction)
      val restore = restoreFunction {globals = globals}
      val () = logs "Performing SSA restore"
      val cleanedFunctions = List.map (optimizedFunctions, restore)
      val () = logs "Done."
   in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = cleanedFunctions,
                 main = main}
   end

end
