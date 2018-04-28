(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CommonBlock (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer

fun transform (Program.T {globals, datatypes, functions, main}) = 
   let
      val shrink = shrinkFunction {globals = globals}

      local 
         fun make transfer = let
                                val l = Label.newNoname ()
                             in
                                Block.T {label = l,
                                         args = Vector.new0 (),
                                         statements = Vector.new0 (),
                                         transfer = transfer}
                             end
      in
         fun makeRaise var = make (Raise (Vector.new1 var))
         fun makeReturn var = make (Return (Vector.new1 var))
         fun makeGoto (dst, var) = make (Goto {dst = dst, args = Vector.new1 var})
      end
      fun makeNullaryGoto dst = Goto {dst = dst, args = Vector.new0 ()}

      val {get = varInfo: 
           Var.t -> {returner: (Func.t * Label.t) option ref,
                     raiser: (Func.t * Label.t) option ref,
                     gotoers: (Func.t * (Label.t * Label.t) list ref) option ref} option,
           set = setVarInfo, ...} =
         Property.getSetOnce
         (Var.plist, Property.initConst NONE)

      val _ = 
         Vector.foreach
         (globals, fn Statement.T {var, ...} => 
          setVarInfo(valOf var, SOME {returner = ref NONE, 
                                      raiser = ref NONE,
                                      gotoers = ref NONE}))

      fun eliminateFunction f = 
         let
            val {args, blocks, mayInline, name, returns, raises, start} =
               Function.dest f
            val newBlocks = ref []
            local
               fun common (sel, make) var = 
                  case varInfo var of 
                     NONE => NONE
                   | SOME varInfo => 
                        let
                           val c = sel varInfo

                           fun install () = 
                              let
                                 val b = make var
                                 val l = Block.label b
                              in
                                 List.push(newBlocks, b) ;
                                 c := SOME (name, l) ;
                                 SOME l
                              end
                        in
                           case !c of 
                              NONE => install ()
                            | SOME (name', l') => 
                                 if Func.equals(name, name')
                                    then SOME l'
                                    else install ()
                        end
            in
               val commonReturner = common (#returner, makeReturn)
               val commonRaiser = common (#raiser, makeRaise)
            end
            fun commonGotoers (k, var) = 
               case varInfo var of 
                  NONE => NONE
                | SOME {gotoers, ...} => 
                     let
                        fun install info = 
                           let
                              val b = makeGoto (k, var)
                              val l = Block.label b
                           in
                              List.push(newBlocks, b) ;
                              List.push(info, (k, l)) ;
                              SOME l
                           end
                        fun install' () = 
                           let
                              val info = ref []
                           in 
                              gotoers := SOME (name, info);
                              install info
                           end
                     in
                        case !gotoers of 
                           NONE => install' ()
                         | SOME (name', info') => 
                              if Func.equals(name, name')
                                 then case List.peek (!info', fn (k', _) => 
                                                      Label.equals(k', k)) of 
                                         NONE => install info'
                                       | SOME (_, l') => SOME l'
                                 else install' ()
                     end

            val blocks = 
               Vector.map
               (blocks, fn Block.T {label, args, statements, transfer} =>
                let
                   val doit = fn SOME l => makeNullaryGoto l
                               | NONE => transfer
                   val transfer = 
                      if Vector.isEmpty statements
                         then case transfer of 
                                 Goto {dst, args = xs} => 
                                    if Vector.length xs = 1
                                       then doit (commonGotoers 
                                                  (dst, Vector.first xs))
                                       else transfer
                               | Return xs => 
                                    if Vector.length xs = 1
                                       then doit (commonReturner 
                                                  (Vector.first xs))
                                       else transfer
                               | Raise xs => 
                                    if Vector.length xs = 1
                                       then doit (commonRaiser
                                                  (Vector.first xs))
                                       else transfer
                               | _ => transfer
                         else transfer
                in
                   Block.T {label = label,
                            args = args,
                            statements = statements,
                            transfer = transfer}
                end)
            val blocks = Vector.concat [Vector.fromList (!newBlocks), blocks]
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
                    functions = List.revMap (functions, eliminateFunction),
                    main = main}
      val _ = Program.clearTop program
   in
      program
   end
end
