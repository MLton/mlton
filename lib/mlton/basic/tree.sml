(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Tree: TREE =
struct

datatype 'a t = T of 'a * 'a t vector

fun children (T (_, v)) = v

fun foldPre (T (a, v), b, f) =
   Vector.fold (v, f (a, b), fn (t, b) =>
		foldPre (t, b, f))

fun foldPost (T (a, v), b, f) =
   f (a, Vector.fold (v, b, fn (t, b) =>
		      foldPost (t, b, f)))

fun traverse (t, f) =
   let
      fun loop (T (a, v)) =
	 let
	    val g = f a
	    val _ = Vector.foreach (v, loop)
	    val _ = g ()
	 in
	    ()
	 end
   in
      loop t
   end

fun foreachPre (t, f) = traverse (t, fn a => (f a; fn () => ()))
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
	    val _ =
	       List.push
	       (nodes, {name = name,
			options = nodeOptions v,
			successors = Vector.toListMap (cs, fn t =>
						       {name = loop t,
							options = []})})
	 in
	    name
	 end
      val _ = loop t
   in
      Dot.layout {nodes = !nodes,
		  options = options,
		  title = title}
   end

fun layout layout' (T (x, ts))
  = let open Layout
    in seq [str "(",
	    layout' x,
	    str ", ",
	    Vector.layout (layout layout') ts,
	    str ")"]
    end

fun map (T (a, ts), f) = T (f a, Vector.map (ts, fn t => map (t, f)))
   
end
