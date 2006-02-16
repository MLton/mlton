(* Written by Henry Cejtin (henry@sourcelight.com). *)

fun print _ = ()

(*
 * My favorite high-order procedure.
 *)
fun fold (lst, folder, state) =
        let fun loop (lst, state) =
                        case lst of
                        [] => state
                        | first::rest => loop (rest, folder (first, state))
        in loop (lst, state)
        end

fun naturalFold (limit, folder, state) =
        if limit < 0
            then raise Domain
            else let fun loop (i, state) =
                             if i = limit
                                 then state
                                 else loop (i+1, folder (i, state))
                 in loop (0, state)
                 end

fun naturalAny (limit, ok) =
        if limit < 0
            then raise Domain
            else let fun loop i =
                             i <> limit andalso
                                 (ok i orelse loop (i+1))
                 in loop 0
                 end

fun naturalAll (limit, ok) =
        if limit < 0
            then raise Domain
            else let fun loop i =
                             i = limit orelse
                                 (ok i andalso loop (i+1))
                 in loop 0
                 end
(*
 * Fold over all permutations.
 * Universe is a list of all the items to be permuted.
 * pFolder is used to build up the permutation.  It is called via
 *      pFolder (next, pState, state, accross)
 * where next is the next item in the permutation, pState is the
 * partially constructed permutation and state is the current fold
 * state over permutations that have already been considered.
 * If pFolder knows what will result from folding over all permutations
 * descending from the resulting partial permutation (starting at state),
 * it should raise the accross exception carrying the new state value.
 * If pFolder wants to continue building up the permutation, it should
 * return (newPState, newState).
 * When a permutation has been completely constructed, folder is called
 * via
 *      folder (pState, state)
 * where pState is the final pState and state is the current state.
 * It should return the new state.
 *)
fun 'a foldOverPermutations (universe, pFolder, pState, folder, state: 'a) =
        let exception accross of 'a
            fun outer (universe, pState, state) =
                    case universe of
                    [] => folder (pState, state)
                    | first::rest =>
                        let fun inner (first, rest, revOut, state) =
                                    let val state =
                                                let val (newPState, state) =
                                                            pFolder (first,
                                                                     pState,
                                                                     state,
                                                                     accross)
                                                in outer (fold (revOut,
                                                                op ::,
                                                                rest),
                                                          newPState,
                                                          state)
                                                end handle accross state => state
                                    in case rest of
                                       [] => state
                                       | second::rest =>
                                           inner (second,
                                                  rest,
                                                  first::revOut,
                                                  state)
                                    end
                        in inner (first, rest, [], state)
                        end
        in outer (universe, pState, state)
        end
(*
 * Fold over all arrangements of bag elements.
 * Universe is a list of lists of items, with equivalent items in the
 * same list.
 * pFolder is used to build up the permutation.  It is called via
 *      pFolder (next, pState, state, accross)
 * where next is the next item in the permutation, pState is the
 * partially constructed permutation and state is the current fold
 * state over permutations that have already been considered.
 * If pFolder knows what will result from folding over all permutations
 * descending from the resulting partial permutation (starting at state),
 * it should raise the accross exception carrying the new state value.
 * If pFolder wants to continue building up the permutation, it should
 * return (newPState, newState).
 * When a permutation has been completely constructed, folder is called
 * via
 *      folder (pState, state)
 * where pState is the final pState and state is the current state.
 * It should return the new state.
 *)
fun 'a foldOverBagPerms (universe, pFolder, pState, folder, state: 'a) =
        let exception accross of 'a
            fun outer (universe, pState, state) =
                    case universe of
                    [] => folder (pState, state)
                    | (fbag as (first::fclone))::rest =>
                        let fun inner (fbag, first, fclone, rest, revOut, state) =
                                    let val state =
                                                let val (newPState, state) =
                                                            pFolder (first,
                                                                     pState,
                                                                     state,
                                                                     accross)
                                                in outer (fold (revOut,
                                                                op ::,
                                                                case fclone of
                                                                [] => rest
                                                                | _ => fclone::rest),
                                                          newPState,
                                                          state)
                                                end handle accross state => state
                                    in case rest of
                                       [] => state
                                       | (sbag as (second::sclone))::rest =>
                                           inner (sbag,
                                                  second,
                                                  sclone,
                                                  rest,
                                                  fbag::revOut,
                                                  state)
                                    end
                        in inner (fbag, first, fclone, rest, [], state)
                        end
        in outer (universe, pState, state)
        end
(*
 * Fold over the tree of subsets of the elements of universe.
 * The tree structure comes from the root picking if the first element
 * is in the subset, etc.
 * eFolder is called to build up the subset given a decision on wether
 * or not a given element is in it or not.  It is called via
 *      eFolder (elem, isinc, eState, state, fini)
 * If this determines the result of folding over all the subsets consistant
 * with the choice so far, then eFolder should raise the exception
 *      fini newState
 * If we need to proceed deeper in the tree, then eFolder should return
 * the tuple
 *      (newEState, newState)
 * folder is called to buld up the final state, folding over subsets
 * (represented as the terminal eStates).  It is called via
 *      folder (eState, state)
 * It returns the new state.
 * Note, the order in which elements are folded (via eFolder) is the same
 * as the order in universe.
 *)
fun 'a foldOverSubsets (universe, eFolder, eState, folder, state: 'a) =
        let exception fini of 'a
            fun f (first, rest, eState) (isinc, state) =
                    let val (newEState, newState) =
                                eFolder (first,
                                         isinc,
                                         eState,
                                         state,
                                         fini)
                    in outer (rest, newEState, newState)
                    end handle fini state => state
            and outer (universe, eState, state) =
                        case universe of
                        [] => folder (eState, state)
                        | first::rest =>
                            let val f = f (first, rest, eState)
                            in f (false, f (true, state))
                            end
            in outer (universe, eState, state)
            end

fun f universe =
        foldOverSubsets (universe,
                         fn (elem, isinc, set, state, _) =>
                             (if isinc
                                  then elem::set
                                  else set,
                              state),
                         [],
                         fn (set, sets) => set::sets,
                         [])
(*
 * Given a partitioning of [0, size) into equivalence classes (as a list
 * of the classes, where each class is a list of integers), and where two
 * vertices are equivalent iff transposing the two is an automorphism
 * of the full subgraph on the vertices [0, size), return the equivalence
 * classes for the graph.  The graph is provided as a connection function.
 * In the result, two equivalent vertices in [0, size) remain equivalent
 * iff they are either both connected or neither is connected to size.
 * The vertex size is equivalent to a vertex x in [0, size) iff
 *      connected (size, y) = connected (x, if y = x then size else y)
 * for all y in [0, size).
 *)
fun refine (size: int,
            classes: int list list,
            connected: int*int -> bool): int list list =
        let fun sizeMatch x =
                (* Check if vertex size is equivalent to vertex x. *)
                    naturalAll (size,
                                fn y => connected (size, y) =
                                            connected (x,
                                                       if y = x
                                                           then size
                                                           else y))
            fun merge (class, (merged, classes)) =
                (* Add class into classes, testing if size should be merged. *)
                    if merged
                        then (true, (rev class)::classes)
                        else let val first::_ = class
                             in if sizeMatch first
                                    then (true, fold (class,
                                                      op ::,
                                                      [size])::classes)
                                    else (false, (rev class)::classes)
                             end
            fun split (elem, (yes, no)) =
                    if connected (elem, size)
                        then (elem::yes, no)
                        else (yes, elem::no)
            fun subdivide (class, state) =
                    case class of
                    [first] => merge (class, state)
                    | _ => case fold (class, split, ([], [])) of
                           ([], no) => merge (no, state)
                           | (yes, []) => merge (yes, state)
                           | (yes, no) => merge (no, merge (yes, state))
        in case fold (classes, subdivide, (false, [])) of
           (true, classes) => rev classes
           | (false, classes) => fold (classes, op ::, [[size]])
        end
(*
 * Given a count of the number of vertices, a partitioning of the vertices
 * into equivalence classes (where two vertices are equivalent iff
 * transposing them is a graph automorphism), and a function which, given
 * two distinct vertices, returns a bool indicating if there is an edge
 * connecting them, check if the graph is minimal.
 * If it is, return
 *      SOME how-many-clones-we-walked-through
 * If not, return NONE.
 * A graph is minimal iff its connection matrix is (weakly) smaller
 * then all its permuted friends, where true is less than false, and
 * the entries are compared lexicographically in the following order:
 *      -
 *      0 -
 *      1 2 -
 *      3 4 5 -
 *      ...
 * Note, the vertices are the integers in [0, nverts).
 *)
fun minimal (nverts: int,
             classes: int list list,
             connected: int*int -> bool): int option =
        let val perm = Array.array (nverts, ~1)
            exception fini
            fun pFolder (new, old, state, accross) =
                    let fun loop v =
                                if v = old
                                    then (Array.update (perm, old, new);
                                          (old + 1, state))
                                    else case (connected (old,
                                                          v),
                                               connected (new,
                                                          Array.sub (perm,
                                                                     v))) of
                                         (true, false) =>
                                             raise (accross state)
                                         | (false, true) =>
                                             raise fini
                                         | _ =>
                                             loop (v + 1)
                    in loop 0
                    end
            fun folder (_, state) =
                    state + 1
        in SOME (foldOverBagPerms (
                     classes,
                     pFolder,
                     0,
                     folder,
                     0)) handle fini => NONE
        end
(*
 * Fold over the tree of graphs.
 *
 * eFolder is used to fold over the choice of edges via
 *      eFolder (from, to, isinc, eState, state, accross)
 * with from > to.
 *
 * If eFolder knows the result of folding over all graphs which agree
 * with the currently made decisions, then it should raise the accross
 * exception carrying the resulting state as a value.
 *
 * To continue normally, it should return the tuple
 *      (newEState, newState)
 *
 * When all decisions are made with regards to edges from `from', folder
 * is called via
 *      folder (size, eState, state, accross)
 * where size is the number of vertices in the graph (the last from+1) and
 * eState is the final eState for edges from `from'.
 *
 * If folder knows the result of folding over all extensions of this graph,
 * it should raise accross carrying the resulting state as a value.
 *
 * If extensions of this graph should be folded over, it should return
 * the new state.
 *)
fun ('a, 'b) foldOverGraphs (eFolder, eState: 'a, folder, state: 'b) =
        let exception noextend of 'b
            fun makeVertss limit =
                    Vector.tabulate (limit,
                                     fn nverts =>
                                             List.tabulate (nverts,
                                                            fn v => v))
            val vertss = ref (makeVertss 0)
            fun findVerts size = (
                    if size >= Vector.length (!vertss)
                        then vertss := makeVertss (size + 1)
                        else ();
                    Vector.sub (!vertss, size))
            fun f (size, eState, state) =
                    let val state =
                                folder (size, eState, state, noextend)
                    in g (size+1, state)
                    end handle noextend state => state
            and g (size, state) =
                    let val indices =
                                findVerts (size - 1)
                        fun SeFolder (to, isinc, eState, state, accross) =
                                eFolder (size-1,
                                         to,
                                         isinc,
                                         eState,
                                         state,
                                         accross)
                        fun Sf (eState, state) =
                                f (size, eState, state)
                    in foldOverSubsets (
                        indices,
                        SeFolder,
                        eState,
                        Sf,
                        state)
                    end
        in f (0, eState, state)
        end

(*
 * Given the size of a graph, a list of the vertices (the integers in
 * [0, size)), and the connected function, check if for all full subgraphs,
 *      3*V - 4 - 2*E >= 0 or V <= 1
 * where V is the number of vertices and E is the number of edges.
 *)
local fun short lst =
        case lst of
        [] => true
        | [_] => true
        | _ => false
in fun okSoFar (size, verts, connected) =
        let exception fini of unit
            fun eFolder (elem, isinc, eState as (ac, picked), _, accross) =
                    (if isinc
                         then (fold (picked,
                                     fn (p, ac) =>
                                         if connected (elem, p)
                                             then ac - 2
                                             else ac,
                                     ac + 3),
                               elem::picked)
                         else eState,
                     ())
            fun folder ((ac, picked), state) =
                    if ac >= 0 orelse short picked
                        then state
                        else raise (fini ())
        in (foldOverSubsets (
               verts,
               eFolder,
               (~4, []),
               folder,
               ());
            true) handle fini () => false
        end
end

fun showGraph (size, connected) =
        naturalFold (size,
                     fn (from, _) => (
                         print ((Int.toString from) ^ ":");
                         naturalFold (size,
                                      fn (to, _) =>
                                          if from <> to andalso connected (from, to)
                                              then print (" " ^ (Int.toString to))
                                              else (),
                                      ());
                         print "\n"),
                     ());

fun showList (start, sep, stop, trans) lst = (
        start ();
        case lst of
        [] => ()
        | first::rest => (
            trans first;
            fold (rest,
                  fn (item, _) => (
                      sep ();
                      trans item),
                  ()));
        stop ())

val showIntList = showList (
                      fn () => print "[",
                      fn () => print ", ",
                      fn () => print "]",
                      fn i => print (Int.toString i))

val showIntListList = showList (
                      fn () => print "[",
                      fn () => print ", ",
                      fn () => print "]",
                      showIntList)

fun h (maxSize, folder, state) =
        let val ctab = Array.tabulate (maxSize,
                                       fn v => Array.array (v, false))
            val classesv = Array.array (maxSize+1, [])
            fun connected (from, to) =
                let val (from, to) = if from > to
                                         then (from, to)
                                         else (to, from)
                in Array.sub (Array.sub (ctab, from), to)
                end
            fun update (from, to, value) =
                let val (from, to) = if from > to
                                         then (from, to)
                                         else (to, from)
                in Array.update (Array.sub (ctab, from), to, value)
                end
            fun triangle (vnum, e) =
                naturalAny (e,
                            fn f => connected (vnum, f)
                                        andalso connected (e, f))
            fun eFolder (from, to, isinc, _, state, accross) =
                    if isinc andalso triangle (from, to)
                        then raise (accross state)
                        else (
                            update (from, to, isinc);
                            ((), state))
            fun Gfolder (size, _, state, accross) = (
                    if size <> 0
                        then Array.update (classesv,
                                           size,
                                           refine (size-1,
                                                   Array.sub (classesv,
                                                              size-1),
                                           connected))
                        else ();
                    case minimal (size, Array.sub (classesv, size), connected) of
                    NONE => raise (accross state)
                    | SOME eatMe =>
                        if okSoFar (size,
                                    List.tabulate (size, fn v => v),
                                    connected)
                            then let val state =
                                         folder (size, connected, state)
                                 in if size = maxSize
                                        then raise (accross state)
                                        else state
                                 end
                            else raise (accross state))
        in foldOverGraphs (eFolder,
                           (),
                           Gfolder,
                           state)
        end

local fun final (size: int, connected: int * int -> bool): int =
        naturalFold (size,
                     fn (from, ac) =>
                         naturalFold (from,
                                      fn (to, ac) =>
                                          if connected (from, to)
                                              then ac - 2
                                              else ac,
                                      ac),
                     3*size - 4)
in fun f maxSize =
        h (maxSize,
           fn (size, connected, state) =>
                   if final (size, connected) = 0
                       then state + 1
                       else state,
           0)
end

fun doOne arg = (
        print (arg ^ " -> ");
        case Int.fromString arg of
        SOME n =>
                print ((Int.toString (f n)) ^ "\n")
        | NONE =>
                print "NOT A NUMBER\n")

   structure Main =
      struct
         fun doit() =
            List.app doOne ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"]

         val doit =
            fn size =>
            let
               fun loop n =
                  if n = 0
                     then ()
                  else (doit();
                        loop(n-1))
            in loop size
            end
      end
