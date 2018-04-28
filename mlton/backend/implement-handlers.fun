(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ImplementHandlers (S: RSSA_TRANSFORM_STRUCTS): RSSA_TRANSFORM = 
struct

open S
open Rssa
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure Function =
   struct
      open Function

      fun hasHandler (f: t): bool =
         let
            val {blocks, ...} = dest f
         in
            Vector.exists
            (blocks, fn Block.T {transfer, ...} =>
             case transfer of
                Transfer.Call
                {return = (Return.NonTail
                           {handler = Handler.Handle _, ...}), ...} =>
                   true
              | _ => false)
         end
   end

structure HandlerLat = FlatLattice (structure Point = Label)

structure ExnStack =
   struct
      local
         structure ZPoint =
            struct
               datatype t = Local | Slot

               val equals: t * t -> bool = op =

               val toString =
                  fn Local => "Local"
                   | Slot => "Slot"

               val layout = Layout.str o toString
            end
         structure L = FlatLattice (structure Point = ZPoint)
      in
         open L
         structure Point = ZPoint
         val locall = point Point.Local
         val slot = point Point.Slot
      end
   end

fun flow (f: Function.t): Function.t =
   if not (Function.hasHandler f)
      then f
   else
   let
      val debug = false
      val {args, blocks, name, raises, returns, start} =
         Function.dest f
      val {get = labelInfo: Label.t -> {global: ExnStack.t,
                                        handler: HandlerLat.t},
           rem, ...} =
         Property.get (Label.plist,
                       Property.initFun (fn _ =>
                                         {global = ExnStack.new (),
                                          handler = HandlerLat.new ()}))
      val _ =
         Vector.foreach
         (blocks, fn Block.T {label, transfer, ...} =>
          let
             val {global, handler} = labelInfo label
             val _ =
                if Label.equals (label, start)
                   then let
                           val _ = ExnStack.<= (ExnStack.slot, global)
                           val _ = HandlerLat.forceTop handler
                        in
                           ()
                        end
                else ()
             fun goto' {global = g, handler = h}: unit =
                let
                   val _ = ExnStack.<= (global, g)
                   val _ = HandlerLat.<= (handler, h)
                in
                   ()
                end
             val goto = goto' o labelInfo
          in
             case transfer of
                Call {return, ...} =>
                   (case return of
                       Return.Dead => ()
                     | Return.NonTail {cont, handler = h} =>
                          let
                             val li as {global = g', handler = h'} =
                                labelInfo cont
                          in
                             case h of
                                Handler.Caller =>
                                   let
                                      val _ = ExnStack.<= (ExnStack.slot, g')
                                      val _ = HandlerLat.<= (handler, h')
                                   in
                                      ()
                                   end
                              | Handler.Dead => goto' li
                              | Handler.Handle l =>
                                   let
                                      fun doit {global = g'', handler = h''} =
                                         let
                                            val _ = ExnStack.<= (ExnStack.locall, g'')
                                            val _ = HandlerLat.<= (HandlerLat.point l, h'')
                                         in
                                            ()
                                         end
                                   in
                                      doit (labelInfo l)
                                      ; doit li
                                   end
                          end
                     | Return.Tail => ())
              | _ => Transfer.foreachLabel (transfer, goto)
          end)
      val _ =
         if debug
            then
               Layout.outputl
               (Vector.layout
                (fn Block.T {label, ...} =>
                 let
                    val {global, handler} = labelInfo label
                 in
                    Layout.record [("label", Label.layout label),
                                   ("global", ExnStack.layout global),
                                   ("handler", HandlerLat.layout handler)]
                 end)
                blocks,
                Out.error)
         else ()
      val blocks =
         Vector.map
         (blocks,
          fn Block.T {args, kind, label, statements, transfer} =>
          let
             val {global, handler} = labelInfo label
             fun setExnStackSlot () =
                if ExnStack.isPointEq (global, ExnStack.Point.Slot)
                   then Vector.new0 ()
                else Vector.new1 SetExnStackSlot
             fun setExnStackLocal () =
                if ExnStack.isPointEq (global, ExnStack.Point.Local)
                   then Vector.new0 ()
                else Vector.new1 SetExnStackLocal
             fun setHandler (l: Label.t) =
                if HandlerLat.isPointEq (handler, l)
                   then Vector.new0 ()
                else Vector.new1 (SetHandler l)
             val post =
                case transfer of
                   Call {return, ...} =>
                      (case return of
                          Return.Dead => Vector.new0 ()
                        | Return.NonTail {handler, ...} =>
                             (case handler of
                                 Handler.Caller => setExnStackSlot ()
                               | Handler.Dead => Vector.new0 ()
                               | Handler.Handle l =>
                                    Vector.concat
                                    [setHandler l, setExnStackLocal ()])
                        | Return.Tail => setExnStackSlot ())
                 | Raise _ => setExnStackSlot ()
                 | Return _ => setExnStackSlot ()
                 | _ => Vector.new0 ()
             val statements = Vector.concat [statements, post]
          in
             Block.T {args = args,
                      kind = kind,
                      label = label,
                      statements = statements,
                      transfer = transfer}
          end)
      val newStart = Label.newNoname ()
      val startBlock =
         Block.T {args = Vector.new0 (),
                  kind = Kind.Jump,
                  label = newStart,
                  statements = Vector.new1 SetSlotExnStack,
                  transfer = Goto {args = Vector.new0 (),
                                   dst = start}}
      val blocks = Vector.concat [blocks, Vector.new1 startBlock]
      val () = Vector.foreach (blocks, rem o Block.label)
   in
      Function.new {args = args,
                    blocks = blocks,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = newStart}
   end

fun transform (Program.T {functions, handlesSignals, main, objectTypes}) =
   Program.T {functions = List.revMap (functions, flow),
              handlesSignals = handlesSignals,
              main = flow main,
              objectTypes = objectTypes}

end
