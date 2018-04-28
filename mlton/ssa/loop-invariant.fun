(* Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * Remove loop invariant args to local loops.
 * fun loop (x, y) = ... loop (x, z) ...
 *
 * becomes
 *
 * fun loop (x, y') =
 *     let fun loop' (y) = ... loop' (z) ...
 *     in loop' (y')
 *     end
 *)

functor LoopInvariant (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer

fun transform (Program.T {globals, datatypes, functions, main}) =
   let
      val shrink = shrinkFunction {globals = globals}

      fun simplifyFunction f =
         let
            val {args, blocks, mayInline, name, raises, returns, start} =
               Function.dest f
            val {get = labelInfo: Label.t -> {callsSelf: bool ref,
                                              visited: bool ref,
                                              invariant: (Var.t * bool ref) vector,
                                              newLabel: Label.t option ref},
                 set = setLabelInfo, ...} =
               Property.getSetOnce
               (Label.plist, 
                Property.initRaise ("LoopInvariant.labelInfo", Label.layout))

            val _ =
               Vector.foreach
               (blocks, fn Block.T {label, args, ...} =>
                setLabelInfo (label,
                              {callsSelf = ref false,
                               visited = ref false,
                               invariant = Vector.map (args, fn (x, _) => 
                                                       (x, ref true)),
                               newLabel = ref NONE}))

            fun visit (Block.T {label, transfer, ...}): unit -> unit =
               let
                  val {visited, ...} = labelInfo label
                  val _ = visited := true
                  val _ =
                     case transfer of
                        Goto {dst, args} =>
                           let
                              val {callsSelf, visited, invariant, ...} = labelInfo dst
                           in
                              if !visited
                                 then (callsSelf := true
                                       ; Vector.foreach2
                                       (args, invariant, fn (x, (y, b)) =>
                                        if !b andalso not (Var.equals (x, y))
                                           then b := false
                                        else ()))
                              else ()
                           end
                      | _ => ()
               in
                  fn () => visited := false
               end
            val _ = Function.dfs (f, visit)
            fun remove (xs: 'a vector, invariant: ('b * bool ref) vector)
               : 'a vector =
               Vector.keepAllMap2 (xs, invariant, fn (x, (_, b)) =>
                                   if !b then NONE else SOME x)

            val newBlocks = ref []
            fun visit (Block.T {label, args, statements, transfer})
               : unit -> unit =
               let
                  val {callsSelf, invariant, newLabel, ...} = labelInfo label
                  val _ =
                     if !callsSelf
                        andalso Vector.exists (invariant, ! o #2)
                        then newLabel := SOME (Label.new label)
                     else ()
                  val transfer = 
                     case transfer of
                        Goto {dst, args} =>
                           let
                              val {invariant, newLabel, ...} = labelInfo dst
                           in
                              case !newLabel of
                                 NONE => transfer
                               | SOME dst' =>
                                    Goto {dst = dst',
                                          args = remove (args, invariant)}
                           end
                      | _ => transfer
                  val (args, statements, transfer) =
                     case !newLabel of
                        NONE => (args, statements, transfer)
                      | SOME label' =>
                           let
                              val _ =
                                 Control.diagnostic
                                 (fn () =>
                                  let open Layout
                                  in seq [Label.layout label,
                                          str " -> ",
                                          Label.layout label']
                                  end)
                              val (outerFormals,
                                   innerFormals,
                                   innerActuals) =
                                 Vector.foldr2
                                 (args, invariant, ([], [], []),
                                  fn ((x, t), (_, b), (ofs, ifs, ias)) =>
                                  if !b
                                     then ((x, t) :: ofs, ifs, ias)
                                  else let val x' = Var.new x
                                       in ((x', t) :: ofs,
                                           (x, t) :: ifs,
                                           x' :: ias)
                                       end)
                           in
                              List.push
                              (newBlocks, 
                               Block.T {label = label',
                                        args = Vector.fromList innerFormals,
                                        statements = statements,
                                        transfer = transfer})
                              ; (Vector.fromList outerFormals, 
                                 Vector.new0 (),
                                 Goto {dst = label',
                                       args = Vector.fromList innerActuals})
                           end
                  val _ = List.push
                          (newBlocks,
                           Block.T {label = label,
                                    args = args,
                                    statements = statements,
                                    transfer = transfer})
               in
                  fn () => newLabel := NONE
               end
            val _ = Function.dfs (f, visit)
            val blocks = Vector.fromList (!newBlocks)
         in
            shrink (Function.new {args = args,
                                  blocks = blocks,
                                  mayInline = mayInline,
                                  name = name,
                                  raises = raises,
                                  returns = returns,
                                  start = start})
         end
      val program =
         Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = List.revMap (functions, simplifyFunction),
                    main = main}
      val _ = Program.clearTop program
   in
      program
   end
end
