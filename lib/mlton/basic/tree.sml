(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
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

fun 'a layoutDot (t: 'a t, {nodeOptions: 'a -> Dot.NodeOption.t list,
                            options,
                            title}) =
   let
      val c = Counter.new 0
      fun next () = concat ["n", Int.toString (Counter.next c)]
      val nodes = ref []
      fun loop (T (v, cs)) =
         let
            val name = next ()
            val () =
               List.push
               (nodes, {name = name,
                        options = nodeOptions v,
                        successors = rev (Seq.fold (cs, [], fn (t, ac) =>
                                                    {name = loop t,
                                                     options = []} :: ac))})
         in
            name
         end
      val _ = loop t
   in
      Dot.layout {nodes = !nodes,
                  options = options,
                  title = title}
   end

fun layout (t, lay) =
   let
      open Layout
      fun loop (T (x, ts)) =
         paren (seq [lay x, str ", ", Seq.layout (ts, loop)])
   in
      loop t
   end

fun map (T (a, ts), f) = T (f a, Seq.map (ts, fn t => map (t, f)))

end

structure Tree = Tree (structure Seq =
                          struct
                             open Vector

                             fun layout (v, l) = Vector.layout l v
                          end)
