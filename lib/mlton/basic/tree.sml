(* Copyright (C) 2013,2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Tree (S: TREE_STRUCTS): TREE =
struct

open S

datatype 'a t = T of 'a * 'a t Seq.t

fun children (T (_, v)) = v

fun foldPre (T (a, v), b, f) =
   Seq.fold (v, f (a, b), fn (t, b) => foldPre (t, b, f))

fun foldPost (T (a, v), b, f) =
   f (a, Seq.fold (v, b, fn (t, b) => foldPost (t, b, f)))

fun traverse (t, f) =
   let
      fun loop (T (a, v)) =
         let
            val g = f a
            val _ = Seq.foreach (v, loop)
            val _ = g ()
         in
            ()
         end
   in
      loop t
   end

fun foreachPre (t, f: 'a -> unit) = traverse (t, fn a => (f a; fn () => ()))
fun foreachPost (t, f) = traverse (t, fn a => fn () => f a)

local
   fun mkLayoutDot {nodeOptions: 'a -> Dot.NodeOption.t list,
                    options,
                    title} =
      let
         fun loopTree (next, nodes, T (x, ts)) =
            let
               val name = next ()
               val () =
                  List.push
                  (nodes, {name = name,
                           options = nodeOptions x,
                           successors = loopForest (next, nodes, ts)})
            in
               name
            end
         and loopForest (next, nodes, ts) =
            rev (Seq.fold (ts, [], fn (t, ac) =>
                           {name = loopTree (next, nodes, t),
                            options = []} :: ac))
         fun wrap (loop, arg) =
            let
               val c = Counter.new 0
               fun next () = concat ["n", Int.toString (Counter.next c)]
               val nodes = ref []
               val _ = loop (next, nodes, arg)
            in
               Dot.layout {nodes = !nodes,
                           options = options,
                           title = title}
            end
      in
         {layoutDotTree = fn t => wrap (loopTree, t),
          layoutDotForest = fn ts => wrap (loopForest, ts)}
      end
in
   fun layoutDotTree (t, opts) = (#layoutDotTree (mkLayoutDot opts)) t
   fun layoutDotForest (ts, opts) = (#layoutDotForest (mkLayoutDot opts)) ts
end
val layoutDot = layoutDotTree

local
   fun mkLayout lay =
      let
         open Layout
         fun layoutTree (T (x, ts)) =
            paren (seq [lay x, str ", ", layoutForest ts])
         and layoutForest ts =
            Seq.layout (ts, layoutTree)
      in
         {layoutTree = layoutTree, layoutForest = layoutForest}
      end
in
   fun layoutTree (t, lay) = (#layoutTree (mkLayout lay)) t
   fun layoutForest (ts, lay) = (#layoutForest (mkLayout lay)) ts
end
val layout = layoutTree

fun map (T (a, ts), f) = T (f a, Seq.map (ts, fn t => map (t, f)))

structure Forest =
struct
   type 'a t = 'a t Seq.t
   val layoutDot = layoutDotForest
   val layout = layoutForest
end

end

structure Tree = Tree (structure Seq =
                          struct
                             open Vector

                             fun layout (v, l) = Vector.layout l v
                          end)
