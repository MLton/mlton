(* Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Change any toplevel function that only calls itself in tail position
 * into one with a local loop and no self calls.
 *)
functor IntroduceLoops (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
datatype z = datatype Exp.t
datatype z = datatype Transfer.t

structure Return =
   struct
      open Return

      fun isTail (z: t): bool =
         case z of
            Dead => false
          | NonTail _ => false
          | Tail => true
   end

fun transform (Program.T {datatypes, globals, functions, main}) =
   let
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val tailCallsItself = ref false
             val _ =
                Vector.foreach
                (blocks, fn Block.T {transfer, ...} =>
                 case transfer of
                    Call {func, return, ...} =>
                       if Func.equals (name, func)
                          andalso Return.isTail return
                          then tailCallsItself := true
                       else ()
                  | _ => ())
             val (args, start, blocks) =
                if !tailCallsItself
                   then
                      let
                         val _ = Control.diagnostics
                            (fn display =>
                             let open Layout
                             in
                                display (Func.layout name)
                             end)
                         val newArgs =
                            Vector.map (args, fn (x, t) => (Var.new x, t))
                         val loopName = Label.newString "loop"
                         val loopSName = Label.newString "loopS"
                         val blocks = 
                            Vector.toListMap
                            (blocks,
                             fn Block.T {label, args, statements, transfer} =>
                             let
                                val transfer =
                                   case transfer of
                                      Call {func, args, return} =>
                                         if Func.equals (name, func)
                                            andalso Return.isTail return
                                            then Goto {dst = loopName, 
                                                       args = args}
                                         else transfer
                                    | _ => transfer
                             in
                                Block.T {label = label,
                                         args = args,
                                         statements = statements,
                                         transfer = transfer}
                             end)
                         val blocks = 
                            Vector.fromList
                            (Block.T 
                             {label = loopSName,
                              args = Vector.new0 (),
                              statements = Vector.new0 (),
                              transfer = Goto {dst = loopName,
                                               args = Vector.map (newArgs, #1)}} ::
                             Block.T 
                             {label = loopName,
                              args = args,
                              statements = Vector.new0 (),
                              transfer = Goto {dst = start,
                                               args = Vector.new0 ()}} ::
                             blocks)
                      in
                         (newArgs,
                          loopSName,
                          blocks)
                      end
                else (args, start, blocks)
          in
             Function.new {args = args,
                           blocks = blocks,
                           mayInline = mayInline,
                           name = name,
                           raises = raises,
                           returns = returns,
                           start = start}
          end)
   in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = functions,
                 main = main}
   end

end
