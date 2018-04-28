(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 2005-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PrePasses (S: PREPASSES_STRUCTS): PREPASSES = 
struct

open S

open Exp Transfer

(* A critical edge is one that connects a block with two or more
 * succesors to one with two or more predecessors.
 * This prepass breaks all critical edges by inserting an eta-block.
 * For some analyses and transformations, simply ensuring the unique
 * successor or predecessor property is sufficient.  (For example, see
 * the comments at the end of "Conditional Constant Propagation" in
 * Section 19.3 of Appel's "Modern Compiler Implementation in ML".) 
 * However, passes that require critical edges to be broken in order
 * to accomodate code motion (for example, PRE), should also break an
 * edge that connects a block with non-goto transfer to one with two
 * or more predecessors.
 *)
structure CriticalEdges =
struct

structure LabelInfo =
   struct
      datatype t = T of {args: (Var.t * Type.t) vector,
                         inDeg: int ref,
                         mustBreak: bool,
                         outDeg: int ref}

      local
         fun make f (T r) = f r
         fun make' f = (make f, ! o (make f))
      in
         val args = make #args
         val (inDeg', inDeg) = make' #inDeg
         val mustBreak = make #mustBreak
         val (outDeg', outDeg) = make' #outDeg
      end

      fun new (args, mustBreak): t = T {args = args, 
                                        inDeg = ref 0, 
                                        mustBreak = mustBreak,
                                        outDeg = ref 0}
   end

fun breakFunction (f, {codeMotion: bool}) =
   let
      val {get = labelInfo: Label.t -> LabelInfo.t, 
           set = setLabelInfo, ...} =
         Property.getSetOnce
         (Label.plist, Property.initRaise ("CriticalEdges.labelInfo", Label.layout))
      val argsLabel = LabelInfo.args o labelInfo
      val inDeg = LabelInfo.inDeg o labelInfo
      val inDeg' = LabelInfo.inDeg' o labelInfo
      val mustBreak = LabelInfo.mustBreak o labelInfo
      val outDeg = LabelInfo.outDeg o labelInfo
      val outDeg' = LabelInfo.outDeg' o labelInfo

      val {args, blocks, mayInline,
           name, raises, returns, start} = Function.dest f

      val _ = 
         Vector.foreach
         (blocks, fn Block.T {args, label, transfer, ...} =>
          let
             val mustBreak =
                case transfer of
                   Bug => false (* no successors *)
                 | Goto _ => false
                 | Raise _ => false (* no successors *)
                 | Return _ => false (* no successors *)
                 | _ => true
          in
             setLabelInfo (label, LabelInfo.new (args, mustBreak))
          end)
      val _ =
         Vector.foreach
         (blocks, fn Block.T {label, transfer, ...} =>
          let
             val outDeg' = outDeg' label
             fun doit l =
                (Int.inc outDeg'
                 ; Int.inc (inDeg' l))
          in
             Transfer.foreachLabel
             (transfer, doit)
          end)

      val newBlocks = ref []
      fun newBlock l =
         let
            val l' = Label.newString "L_crit"
            val args =
               Vector.map
               (argsLabel l, fn (x, ty) =>
                (Var.new x, ty))
            val _ =
               List.push
               (newBlocks,
                Block.T {args = args,
                         label = l',
                         statements = Vector.new0 (),
                         transfer = Goto {dst = l,
                                          args = Vector.map(args, #1)}})
         in
            l'
         end
      val blocks =
         Vector.map
         (blocks, fn b as Block.T {args, label, statements, transfer} =>
          if (codeMotion andalso mustBreak label) 
             orelse outDeg label >= 2
             then let
                     fun doit t =
                        Transfer.replaceLabel
                        (t, fn l =>
                         if inDeg l > 1 
                            then newBlock l
                            else l)
                  in
                     Block.T {args = args,
                              label = label,
                              statements = statements,
                              transfer = doit transfer}
                  end
             else b)
   in
      Function.new {args = args,
                    blocks = Vector.concat [blocks, Vector.fromList (!newBlocks)],
                    mayInline = mayInline,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

fun break (Program.T {datatypes, globals, functions, main}, codeMotion) =
   let
      val functions = 
         List.revMap (functions, fn f => 
                      breakFunction (f, codeMotion))
   in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = functions,
                 main = main}
   end
end

val breakCriticalEdgesFunction = CriticalEdges.breakFunction
(* quell unused warning *)
val _ = breakCriticalEdgesFunction
val breakCriticalEdges = CriticalEdges.break

structure DeadBlocks =
struct

fun eliminateFunction f =
   let
      val {args, blocks, mayInline, name, raises, returns, start} =
         Function.dest f
      val {get = isLive, set = setLive, rem} =
         Property.getSetOnce (Label.plist, Property.initConst false)
      val _ = Function.dfs (f, fn Block.T {label, ...} =>
                            (setLive (label, true)
                             ; fn () => ()))
      val f =
         if Vector.forall (blocks, isLive o Block.label)
            then f
         else
            let 
               val blocks =
                  Vector.keepAll
                  (blocks, isLive o Block.label)
            in
               Function.new {args = args,
                             blocks = blocks,
                             mayInline = mayInline,
                             name = name,
                             raises = raises,
                             returns = returns,
                             start = start}
            end
       val _ = Vector.foreach (blocks, rem o Block.label)
   in
     f
   end

fun eliminate (Program.T {datatypes, globals, functions, main}) =
   Program.T {datatypes = datatypes,
              globals = globals,
              functions = List.revMap (functions, eliminateFunction),
              main = main}
end

val eliminateDeadBlocksFunction = DeadBlocks.eliminateFunction
val eliminateDeadBlocks = DeadBlocks.eliminate


structure Order =
struct

fun orderFunctions (p as Program.T {globals, datatypes, main, ...}) =
   let
      val functions = ref []
      val () =
         Program.dfs
         (p, fn f =>
          let
             val {args, mayInline, name, raises, returns, start, ...} =
                Function.dest f
             val blocks = ref []
             val () =
                Function.dfs
                (f, fn b =>
                 (List.push (blocks, b)
                  ; fn () => ()))
             val f = Function.new {args = args,
                                   blocks = Vector.fromListRev (!blocks),
                                   mayInline = mayInline,
                                   name = name,
                                   raises = raises,
                                   returns = returns,
                                   start = start}
          in
             List.push (functions, f)
             ; fn () => ()
          end)
   in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = List.rev (!functions),
                 main = main}
   end

end

val orderFunctions = Order.orderFunctions


structure Reverse =
struct

fun reverseFunctions (Program.T {globals, datatypes, functions, main}) =
   Program.T {datatypes = datatypes,
              globals = globals,
              functions = List.rev functions,
              main = main}
end

val reverseFunctions = Reverse.reverseFunctions

end
