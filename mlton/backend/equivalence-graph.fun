(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor EquivalenceGraph (S: EQUIVALENCE_GRAPH_STRUCTS): EQUIVALENCE_GRAPH = 
struct

open S

structure Set = DisjointSet
structure Plist = PropertyList

structure Class =
   struct
      datatype t = T of {plist: Plist.t,
                         size: int ref} Set.t

      local
         fun make sel (T s) = sel (Set.! s)
      in
         val plist = make #plist
         val size = make (! o #size)
      end

      fun setSize (T s, n) = #size (Set.! s) := n

      fun new (size: int): t =
         T (Set.singleton {plist = Plist.new (),
                           size = ref size})

      fun == (c as T s, T s') =
         if Set.equals (s, s')
            then ()
         else
            let
               val {size = ref n, ...} = Set.! s
               val {size = ref n', ...} = Set.! s'
            in
               Set.union (s, s')
               ; setSize (c, n + n')
            end
   end

datatype t = T of {classes: Class.t list ref,
                   edges: (Class.t * Class.t) list ref}

fun new () = T {classes = ref [],
                edges = ref []}

fun newClass (T {classes, ...}, {size}) =
   let
      val c = Class.new size
      val _ = List.push (classes, c)
   in
      c
   end

fun addEdge (T {edges, ...}, c, c') =
   List.push (edges, (c, c'))

fun == (_, c, c') = Class.== (c, c')

fun coarsen (T {classes, edges, ...}, {maxClassSize}) =
   let
      (* Combine classes with an edge between them where possible. *)
      val _ =
         List.foreach (!edges, fn (c, c') =>
                       if Class.size c + Class.size c' <= maxClassSize
                          then Class.== (c, c')
                       else ())
      (* Get a list of all classes without duplicates. *)
      val {get, ...} =
         Property.get (Class.plist, Property.initFun (fn _ => ref false))
      val classes =
         List.fold
         (!classes, [], fn (class, ac) =>
          let
             val r = get class
          in
             if !r
                then ac
             else (r := true
                   ; class :: ac)
          end)
      (* Sort classes in decreasing order of size. *)
      val classes =
         QuickSort.sortList (classes, fn (c, c') =>
                             Class.size c >= Class.size c')
      (* Combine classes where possible. *)
      fun loop (cs: Class.t list): unit =
         case cs of
            [] => ()
          | c :: cs =>
               loop
               (rev
                (List.fold
                 (cs, [], fn (c', ac) =>
                  if Class.size c  + Class.size c' <= maxClassSize
                     then (Class.== (c, c')
                           ; ac)
                  else c' :: ac)))
      val _ = loop classes
   in
      ()
   end

end

structure EquivalenceGraph = EquivalenceGraph ()
