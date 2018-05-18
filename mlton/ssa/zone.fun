(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Zone (S: SSA2_TRANSFORM_STRUCTS): SSA2_TRANSFORM = 
struct

open S

structure Graph = DirectedGraph
local
   open Graph
in
   structure Node = Node
end

structure Scope = UniqueId ()

fun zoneFunction f =
   let
      val {args, mayInline, name, raises, returns, start, ...} = Function.dest f
      datatype z = datatype Exp.t
      datatype z = datatype Statement.t
      val {get = labelInfo: Label.t -> {isInLoop: bool ref,
                                        isCut: bool ref}, ...} =
         Property.get (Label.plist,
                       Property.initFun (fn _ => {isCut = ref false,
                                                  isInLoop = ref false}))
      (* Mark nodes that are in loops so that we can avoid inserting tuple
       * constructions there.
       *)
      val {graph, nodeBlock, ...} = Function.controlFlow f
      val () =
         List.foreach
         (Graph.stronglyConnectedComponents graph, fn ns =>
          let
             fun doit () =
                List.foreach
                (ns, fn n =>
                 #isInLoop (labelInfo (Block.label (nodeBlock n))) := true)
          in
             case ns of
                [n] => if Node.hasEdge {from = n, to = n}
                          then doit ()
                       else ()
              | _ => doit ()
          end)
      val dominatorTree = Function.dominatorTree f
      (* Decide which labels to cut at. *)
      val cutDepth = !Control.zoneCutDepth
      fun addCuts (Tree.T (b, ts), depth: int) =
         let
            val depth =
               if depth = 0
                  then
                     let
                        val Block.T {label, ...} = b
                        val {isCut, isInLoop, ...} = labelInfo label
                        val () =
                           if !isInLoop
                              then
                                 Control.diagnostic
                                 (fn () =>
                                  let
                                     open Layout
                                  in
                                     seq [str "skipping cut at ",
                                          Label.layout label]
                                  end)
                           else isCut := true
                     in
                        cutDepth
                     end
               else depth - 1
         in
            Vector.foreach (ts, fn t => addCuts (t, depth))
         end
      val () = addCuts (dominatorTree, cutDepth)
      (* Build a tuple of lives at each cut node. *)
      type info = {componentsRev: Var.t list ref,
                   numComponents: int ref,
                   scope: Scope.t,
                   tuple: Var.t}
      fun newInfo () =
         {componentsRev = ref [],
          numComponents = ref 0,
          scope = Scope.new (),
          tuple = Var.newNoname ()}
      datatype varInfo =
         Global
        | Local of {blockCache: Var.t option ref,
                    defScope: Scope.t,
                    ty: Type.t,
                    uses: {exp: Exp.t,
                           scope: Scope.t} list ref}
      val {get = varInfo: Var.t -> varInfo,
           set = setVarInfo, ...} =
         Property.getSetOnce (Var.plist,
                              Property.initFun (fn _ => Global))
      val blockSelects: {blockCache: Var.t option ref,
                         statement: Statement.t} list ref = ref []
      fun addBlockSelects (ss: Statement.t vector): Statement.t vector =
         let
            val blockSelectsV = Vector.fromList (!blockSelects)
            val () = Vector.foreach (blockSelectsV, fn {blockCache, ...} =>
                                     blockCache := NONE)
            val () = blockSelects := []
         in
            Vector.concat [Vector.map (blockSelectsV, #statement), ss]
         end
      fun define (x: Var.t, ty: Type.t, info: info): unit =
         setVarInfo (x, Local {blockCache = ref NONE,
                               defScope = #scope info,
                               ty = ty,
                               uses = ref []})
      fun replaceVar (x: Var.t,
                      {componentsRev, numComponents, scope, tuple}: info)
         : Var.t =
         case varInfo x of
            Global => x
          | Local {blockCache, defScope, ty, uses, ...} =>
               case !blockCache of
                  SOME y => y
                | _ => 
                     if Scope.equals (defScope, scope)
                        then x
                     else
                        let
                           fun new () =
                              let
                                 val offset = !numComponents
                                 val () = List.push (componentsRev, x)
                                 val () = numComponents := 1 + offset
                                 val exp = Select {base = Base.Object tuple,
                                                   offset = offset}
                                 val () = List.push (uses, {exp = exp,
                                                            scope = scope})
                              in
                                 exp
                              end
                           val exp =
                              case !uses of
                                 [] => new ()
                               | {exp, scope = scope'} :: _ =>
                                    if Scope.equals (scope, scope')
                                       then exp
                                    else new ()
                           val y = Var.new x
                           val () = blockCache := SOME y
                           val () =
                              List.push
                              (blockSelects,
                               {blockCache = blockCache,
                                statement = Bind {exp = exp,
                                                  ty = ty,
                                                  var = SOME y}})
                        in
                           y
                        end
      val blocks = ref []
      fun loop (Tree.T (b, ts), info: info) =
         let
            val Block.T {args, label, statements, transfer} = b
            val {isCut = ref isCut, ...} = labelInfo label
            val info' = 
               if isCut
                  then newInfo ()
               else info
            val define = fn (x, t) => define (x, t, info')
            val () = Vector.foreach (args, define)
            val statements =
               Vector.map
               (statements, fn s =>
                let
                   val s = Statement.replaceUses (s, fn x =>
                                                  replaceVar (x, info'))
                   val () = Statement.foreachDef (s, define)
                in
                   s
                end)
            val transfer =
                Transfer.replaceVar (transfer, fn x => replaceVar (x, info'))
            val statements = addBlockSelects statements
            val () = Vector.foreach (ts, fn t => loop (t, info'))
            val statements =
               if not isCut
                  then statements
               else
                  let
                     val {componentsRev, tuple, ...} = info'
                     val components = Vector.fromListRev (!componentsRev)
                  in
                     if Vector.isEmpty components
                        then statements
                     else
                        let
                           val componentTys =
                              Vector.map
                              (components, fn x =>
                               case varInfo x of
                                  Global => Error.bug "Zone.zoneFunction: global component"
                                | Local {ty, uses, ...} =>
                                     (ignore (List.pop uses)
                                      ; {elt = ty,
                                         isMutable = false}))
                           val components =
                              Vector.map (components, fn x =>
                                          replaceVar (x, info))
                           val s =
                              Bind
                              {exp = Object {args = components, con = NONE},
                               ty = Type.tuple (Prod.make componentTys),
                               var = SOME tuple}
                        in
                           addBlockSelects (Vector.concat [Vector.new1 s,
                                                           statements])
                        end
                  end
            val () = List.push (blocks,
                                Block.T {args = args,
                                         label = label,
                                         statements = statements,
                                         transfer = transfer})
         in
            ()
         end
      val () = loop (dominatorTree, newInfo ())
      val blocks = Vector.fromList (!blocks)
   in
      Function.new {args = args,
                    blocks = blocks,
                    mayInline = mayInline,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

fun maybeZoneFunction (f, ac) =
   let
      val {blocks, name, ...} = Function.dest f
      val () =
         Control.diagnostic
         (fn () =>
          let
             open Layout
          in
             seq [Func.layout name, str " has ", str " blocks."]
          end)
   in
      if Vector.length blocks <= !Control.maxFunctionSize
         then f :: ac
      else zoneFunction f :: ac
   end

fun transform2 (Program.T {datatypes, globals, functions, main}) =
   Program.T {datatypes = datatypes,
              globals = globals,
              functions = List.fold (functions, [], maybeZoneFunction),
              main = main}

end
