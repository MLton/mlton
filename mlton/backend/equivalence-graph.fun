(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor EquivalenceGraph(S: EQUIVALENCE_GRAPH_STRUCTS): EQUIVALENCE_GRAPH = 
struct

open S

structure Set = DisjointSet
structure Plist = PropertyList

(* A simple implementation where greedy doesn't do anything *)
structure Class =
   struct
      type t = Plist.t Set.t
      val plist = Set.value
   end
type t = unit
fun new() = ()
fun newClass _ = Set.singleton(PropertyList.new())
fun addEdge _ = ()
fun ==(_, c, c') = Set.union(c, c')
fun greedy _ = ()

(* A slightly trickier implementation where greedy just walks over the edges
 * in order.
 *)
structure Class =
   struct
      datatype t = T of {size: int ref,
			 plist: Plist.t} Set.t

      local
	 fun make sel (T s) = sel(Set.value s)
      in val plist = make #plist
	 val size = make (! o #size)
      end

      fun setSize(T s, n) = #size(Set.value s) := n

      fun new(size: int): t = T(Set.singleton{size = ref size,
					      plist = Plist.new()})

      fun ==(c as T s, T s') =
	 if Set.equals(s, s')
	    then ()
	 else let val {size = ref n, plist} = Set.value s
		  val {size = ref n', plist} = Set.value s'
	      in Set.union(s, s')
		 ; setSize(c, n + n')
	      end
   end

datatype t = T of {edges: (Class.t * Class.t) list ref}

fun new() = T{edges = ref []}

fun newClass(_, n) = Class.new n

fun addEdge(T{edges, ...}, {from, to}) =
   List.push(edges, (from, to))

fun ==(_, c, c') = Class.==(c, c')

fun greedy{graph = T{edges, ...}, maxClassSize} =
   List.foreach(!edges, fn (c, c') =>
		if Class.size c + Class.size c' <= maxClassSize
		   then Class.==(c, c')
		else ())

(*
 * Given an edge, return how desirable it is to merge the endpoints
 * of the edge.  The result is an int option because we return
 * NONE if they are not mergable.
 * Note, it looks at the details inside Class, but the whole thing
 * is just a hack.
 *)
fun goodness (Class.T lhs: Class.t, Class.T rhs: Class.t): int option =
       if Set.equals (lhs, rhs)
	  then NONE
	  else let val {size = ref lsize, ...} = Set.value lhs
		   val {size = ref rsize, ...} = Set.value rhs
	       in SOME (~ (lsize + rsize))
	       end

fun findBest (edges: (Class.t * Class.t) list)
	     : (Class.t * Class.t) option =
       let fun folder (e: Class.t * Class.t,
                       ac: (int * (Class.t * Class.t)) option) =
                  case goodness e of
                     NONE => ac
                     | SOME g =>
                          case ac of
                             NONE => SOME (g, e)
                             | SOME (g', _) =>
                                  if g > g'
                                     then SOME (g, e)
                                     else ac
       in case List.fold (edges, NONE, folder) of
	     NONE => NONE
	     | SOME (goodness, e) => (
(* 					print ("\nHCC:\tgoodness " ^
 * 					       Int.toString goodness ^
 * 					       "\n");
 *)
					SOME e
				     )
       end

fun greedy' {graph = T {edges, ...}, maxClassSize} =
       let fun loop () =
		  case findBest (! edges) of
		     NONE => ()
		     | SOME (lhs, rhs) =>
			  if Class.size lhs + Class.size rhs <= maxClassSize
			     then (
				     Class.== (lhs, rhs);
				     loop ()
				  )
			     else ()
       in loop ()
       end

end

structure EquivalenceGraph = EquivalenceGraph()
