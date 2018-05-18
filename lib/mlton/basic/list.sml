(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure List: LIST =
struct

structure Int = Pervasive.Int
structure F =
   struct
      type 'a t = 'a list

      fun fold (l, b, f) =
         let
            fun loop (l, b) =
               case l of
                  [] => b
                | x :: l => loop (l, f (x, b))
         in loop (l, b)
         end
   end
structure F = Fold (open F
                    type 'a elt = 'a)
open F

fun dropPrefix (l: 'a t, n: int): 'a t =
   if n = 0
      then l
   else
      case l of
         [] => Error.bug "List.dropPrefix"
       | _ :: l => dropPrefix (l, n - 1)

fun dropSuffix (l, n: int) = rev (dropPrefix (rev l, n))

fun nth (l, i: int) =
   let
      fun loop (l, i) =
         case l of
            [] => Error.bug "List.nth"
          | x :: l =>
               if i = 0
                  then x
               else loop (l, i - 1)
   in
      if i < 0
         then Error.bug "List.nth"
      else loop (l, i)
   end

fun exists (l, f) =
   let
      fun loop l =
         case l of
            [] => false
          | x :: l => f x orelse loop l
   in
      loop l
   end

fun first l =
   case l of
      [] => Error.bug "List.first"
    | x :: _ => x

fun forall (l, f) =
   let
      fun loop l =
         case l of
            [] => true
          | x :: l => f x andalso loop l
   in
      loop l
   end

fun foralli (l, f) =
   let
      fun loop (l, i: int) =
         case l of
            [] => true
          | x :: l => f (i, x) andalso loop (l, i + 1)
   in
      loop (l, 0)
   end

fun fold2 (l1, l2, b, f) =
   let
      fun loop (l1, l2, b) =
         case (l1,       l2)       of
              ([],       [])       => b
            | (x1 :: l1, x2 :: l2) => loop (l1, l2, f (x1, x2, b))
            | _                    => Error.bug "fold2"
   in loop (l1, l2, b)
   end

fun fold3 (l1, l2, l3, b, f) =
   let
      fun loop (l1, l2, l3, b) =
         case (l1,       l2,       l3)       of
              ([],       [],       [])       => b
            | (x1 :: l1, x2 :: l2, x3 :: l3) =>
                 loop (l1, l2, l3, f (x1, x2, x3, b))
            | _                    => Error.bug "fold3"
   in loop (l1, l2, l3, b)
   end

fun foreach2 (l1, l2, f) = fold2 (l1, l2, (), fn (x1, x2, ()) => f (x1, x2))

fun index (l, f) =
   let
      fun loop (l, i: int) =
         case l of
            [] => NONE
          | x :: l => if f x then SOME i else loop (l, i + 1)
   in
      loop (l, 0)
   end

fun isEmpty l =
   case l of
      [] => true
    | _ :: _ => false

fun peek (l, f) =
   let
      fun loop l =
         case l of
            [] => NONE
          | x :: l => if f x then SOME x else loop l
   in
      loop l
   end

fun peeki (l, f) =
   let
      fun loop (l, i: int) =
         case l of
            [] => NONE
          | x :: l => if f (i, x) then SOME (i, x) else loop (l, i + 1)
   in
      loop (l, 0)
   end

fun peekMap (l, f) =
   let
      fun loop l =
         case l of
            [] => NONE
          | x :: l =>
               (case f x of
                   NONE => loop l
                 | SOME x => SOME x)
   in
      loop l
   end

fun appendRev (l, l') = fold (l, l', op ::)

fun append (l, l') = appendRev (rev l, l')

fun rev l = appendRev (l, [])

fun foldr (l, b, f) = fold (rev l, b, f)

fun foldr2 (l1, l2, b, f) = fold2 (rev l1, rev l2, b, f)

fun revMap (l, f) = fold (l, [], fn (x, l) => f x :: l)

fun map (l, f) = rev (revMap (l, f))

fun map2 (l1, l2, f) =
   rev (fold2 (l1, l2, [], fn (x1, x2, l) => f (x1, x2) :: l))

fun map3 (l1, l2, l3, f) =
   rev (fold3 (l1, l2, l3, [], fn (x1, x2, x3, l) => f (x1, x2, x3) :: l))

local
   fun make (l1, l2, f, unequal) =
      let
         val rec loop =
            fn ([],     [])         => true
             | (x1 :: l1, x2 :: l2) => f (x1, x2) andalso loop (l1, l2)
             | _                    => unequal ()
      in loop (l1, l2)
      end
in
   fun forall2 (l1, l2, f) = make (l1, l2, f, fn () => Error.bug "forall2")
   fun equals (l1, l2, f) = make (l1, l2, f, fn () => false)
end

val cons = op ::

val snoc = fn (l, x) => l @ [x]

fun compare (l, l', comp) =
   let
      val rec compare =
         fn ([],     [])       => EQUAL
          | (_,      [])       => GREATER
          | ([],     _)        => LESS
          | (x :: l, x' :: l') => (case comp (x, x') of
                                      EQUAL => compare (l, l')
                                    | r => r)
   in compare (l, l')
   end

fun allButLast l =
   case rev l of
      _ :: l => rev l
    | _ => Error.bug "List.allButLast"

fun zip (l1, l2) = foldr2 (l1, l2, [], fn (x1, x2, l) => (x1, x2) :: l)

fun unzip l = foldr (l, ([], []), fn ((x1, x2), (l1, l2)) =>
                    (x1 :: l1, x2 :: l2))

fun concatRev l = fold (l, [], append)

fun concat l = concatRev (rev l) 

fun concatMap (l, f) = concatRev (revMap (l, f))

val ('a, 'b) unfoldri: int * 'a * (int * 'a -> 'b * 'a) -> 'b list =
   fn (n, a, f) =>
   if n < 0
      then Error.bug "List.unfoldri"
   else
      let
         fun loop (i, a, bs) =
            if i < 0
               then bs
            else
               let
                  val (b, a') = f (i, a)
               in
                  loop (i - 1, a', b :: bs)
               end
      in
         loop (n - 1, a, [])
      end

val ('a, 'b) unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b list =
   fn (n, a, f) =>
   if n < 0
      then Error.bug "List.unfoldi"
   else
      let
         fun loop (i, a, bs) =
            if i >= n
               then rev bs
            else
               let
                  val (b, a') = f (i, a)
               in
                  loop (i + 1, a', b :: bs)
               end
      in
         loop (0, a, [])
      end

fun unfoldr (a, f) =
   let
      fun loop (a, bs) =
         case f a of
            NONE => bs
          | SOME (b, a') => loop (a', b :: bs)
   in
      loop (a, [])
   end

fun unfold (a, f) = rev (unfoldr (a, f))

fun tabulate (n: int, f) = unfoldri (n, (), fn (i, ()) => (f i, ()))

fun new (n, x) = tabulate (n, fn _ => x)

fun duplicate (n, f) = tabulate (n, fn _ => f ())

fun contains (l, x, equals) = exists (l, fn x' => equals (x, x'))

fun union (l, l', equals) =
   fold (l', l, fn (x, l) => if contains (l, x, equals) then l else x :: l)

fun pop r =
   case !r of
      [] => Error.bug "List.pop"
    | x :: l => (r := l; x)

fun push (r, x) = r := x :: !r

fun remFst (l, f, notFound) =
   let
      fun loop (l, ac) =
         case l of
            [] => notFound ()
          | x :: l =>
               if f x
                  then appendRev (ac, l)
               else loop (l, x :: ac)
   in loop (l, [])
   end

fun removeFirst (l, f) = remFst (l, f, fn () => Error.bug "List.removeFirst")

fun remove (l, f) = remFst (l, f, fn () => l)

fun nthTail (l, n: int) =
   let
      fun loop (l, i) =
         if i <= 0
            then l
         else (case l of
                  [] => Error.bug "List.nthTail"
                | _ :: l => loop (l, i - 1))
   in loop (l, n)
   end

fun firstN (l, n: int) =
   let
      fun loop (l, i, ac) =
         if i <= 0
            then rev ac
         else (case l of
                  [] => Error.bug "List.firstN"
                | x :: l => loop (l, i - 1, x :: ac))
   in loop (l, n, [])
   end

fun 'a ordered {<= : 'a * 'a -> bool} =
   let
      fun insert (l, x) =
         let
            fun loop (l, ac) =
               case l of
                  [] => appendRev (ac, [x])
                | x' :: l' =>
                     if x <= x'
                        then appendRev (ac, x :: l)
                     else loop (l', x' :: ac)
         in loop (l, [])
         end

      fun insertionSort l = fold (l, [], fn (x, ac) => insert (ac, x))

      fun partition (ns, x) =
         fold (ns, ([], []),
              fn (y, (left, right)) =>
              if x <= y then (left, cons (y, right))
              else (cons (y, left), right))

      local  
         val columnSize: int = 5
         val sort = insertionSort
         fun breakIntoColumns ns =
            if Int.< (length ns, columnSize)
               then cons (ns, [])
            else cons (firstN (ns, columnSize),
                      breakIntoColumns (nthTail (ns, columnSize)))
      in
         fun median ns = orderStatistic (ns, Int.quot (length ns, 2))
         and orderStatistic (ns, i) =
            if Int.< (length ns, columnSize)
               then nth (sort ns, i)
            else let
                    val medianOfMedians =
                       median (map (breakIntoColumns ns, columnMedian))
                    val (left, right) = partition (ns, medianOfMedians)
                    val numLeft = length left
                 in if Int.< (i, numLeft)
                       then orderStatistic (left, i)
                    else orderStatistic (right, i - numLeft)
                 end
         and columnMedian ns =
            nth (sort ns, Int.quot (length ns, 2))
      end    

      fun choose (op <) (s, n) =
         let fun insert (x, s) =
            let
               fun insert (m, s) =
                  if m >= n
                     then []
                  else (case s of
                           [] => [x]
                         | y :: s => if x < y then x :: s
                                     else y :: insert (m + 1, s))
            in insert (0, s)
            end
         in firstN (fold (s, [], insert),n)
         end

      val smallest = choose (op < : int * int -> bool)
      val largest = choose (op > : int * int -> bool)

      fun getFirst (l, extreme, name) =
         case extreme (l, 1) of
            x :: _ => x
          | _ => Error.bug name

      fun min l = getFirst (l, smallest, "List.ordered.min")
      fun max l = getFirst (l, largest, "List.ordered.max")

   in {insert = insert,
       insertionSort = insertionSort,
       median = median,
       orderStatistic = orderStatistic,
       partition = partition,
       max = max,
       min = min,
       largest = largest,
       smallest = smallest}
   end

fun insertionSort (l, le) = #insertionSort (ordered {<= = le}) l

fun splitLast l =
   case rev l of
      [] => Error.bug "List.splitLast"
    | x :: l => (rev l, x)

fun power l =
   case l of
      [] => [[]]
    | x :: l => let val rest = power l
                in rest @ map (rest, fn s => x :: s)
                end

fun equalsAsSet (l1, l2, equals) =
   let fun subset (l, l') = forall (l, fn x => exists (l', fn x' => equals (x, x')))
   in subset (l1, l2) andalso subset (l2, l1)
   end

fun 'a set {equals: 'a * 'a -> bool,
            layout: 'a -> Layout.t} =
   let
      val equal = equals
      fun equalTo x x' = equal (x, x')
      fun contains (s, x) = exists (s, equalTo x)
      fun s <= s' = forall (s, fn x => contains (s', x))
      fun s' >= s = s <= s'
      val equals = fn (s, s') => s <= s' andalso s' <= s
      fun s < s' = s <= s' andalso exists (s', fn x => not (contains (s, x)))
      fun s' > s = s < s'
      fun singleton (x: 'a) = cons (x, [])
      fun add (s, x) = if contains (s, x) then s else cons (x, s)
      fun areDisjoint (s, s') = forall (s, fn x => not (contains (s', x)))
(*      val subset = keepAll *)
      fun subset (s: 'a t, p) =
         fold (s, [], fn (x, s'') => if p x then x::s'' else s'')
      fun subsetSize (s: 'a t, p) =
         fold (s, 0: int, fn (x, n) => if p x then n + 1 else n)
      fun s - s' = subset (s, fn x => not (contains (s', x)))
(*      fun s + s' = append (s - s', s') *)
      fun s + s' =
         fold (s, s', fn (x, s'') => if not (contains (s', x)) then x::s'' else s'')
      fun intersect (s, s') = subset (s, fn x => contains (s', x))
      fun unions ss = fold (ss, [], op +)
      val size: 'a t -> int = length

      val layout = fn vs =>
         let open Layout
         in seq [str "{",
                align (separateRight (map (vs, layout), ", ")),
                str "}"]
         end

      val remove = fn (s, x) => remove (s, equalTo x)

      fun replace (s: 'a t, f): 'a t =
         fold (s, [], fn (x, s) => (case f x of
                                        NONE => s
                                      | SOME y => add (s, y)))
      fun map (s: 'a t, f) = replace (s, fn x => SOME (f x))

   in {empty = [],
       singleton = singleton,
       size = size,
       equals = equals,
       <= = op <=,
       >= = op >=,
       < = op <,
       > = op >,
       + = op +,
       - = op -,
       intersect = intersect,
       unions = unions,
       add = add,
       remove = remove,
       contains = contains,
       areDisjoint = areDisjoint,
       subset = subset,
       subsetSize = subsetSize,
       map = map,
       replace = replace,
       layout = layout}
   end

fun subsets (s, n) =
   let
      fun subs (s, size, n, elts, accum) =
         if n <= 0
            then cons (elts, accum)
         else (case s of
                  x :: xs =>
                     if size <= n
                        then cons (cons (x, append (xs, elts)), accum)
                     else subs (xs, size - 1, n - 1,
                                cons (x, elts),
                                subs (xs, size - 1, n, elts , accum))
                | _ => Error.bug "List.subsets")
   in subs (s, length s, n, [], [])
   end

fun appendMap (l1, f, l2) = appendRev (revMap (l1, f), l2)

fun separate (ts, s) =
   case ts of
      [] => []
    | t :: ts => t :: (let 
                           val rec loop =
                              fn [] => []
                               | t :: ts => s :: t:: (loop ts)
                       in loop ts
                       end)

fun cross l =
   case l of
      [] => [[]]
    | x :: l =>
         let val rest = cross l
         in concatMap (x, fn x => map (rest, fn t => x :: t))
         end

fun removeDuplicates (l, equals) =
   fold (l, [], fn (x, ac) =>
         if contains (ac, x, equals)
            then ac
         else x :: ac)

fun removeCommonPrefix (l1, l2, equals) =
   let
      fun loop (arg as (l1, l2)) =
         case (l1, l2) of
            (_, []) => arg
          | ([], _) => arg
          | (x1 :: l1, x2 :: l2) =>
               if equals (x1, x2)
                  then loop (l1, l2)
               else arg
   in loop (l1, l2)
   end

fun removePrefix (l, p) =
   let
      fun loop l =
         case l of
            [] => []
          | x :: l' => if p x then loop l' else l
   in loop l
   end

fun isPrefix (l, l', f) =
   let
      val rec loop =
         fn ([], _) => true
          | (_, []) => false
          | (x :: l, x' :: l') => f (x, x') andalso loop (l, l')
   in loop (l, l')
   end

fun insert (xs, x, op <=) = #insert (ordered {<= = op <=}) (xs, x)

fun toString xToString l =
   Layout.toString (layout (Layout.str o xToString) l)

val splitAt: 'a t * int -> 'a t * 'a t =
   fn (l, i) =>
   let
      fun loop (i, ac, l) =
         if i = 0
            then (rev ac, l)
            else
               case l of
                  [] => Error.bug "List.splitAt"
                | x :: l => loop (i - 1, x :: ac, l)
   in
      loop (i, [], l)
   end

fun splitPrefix (l, p) =
   let
      fun loop (l, ac) =
         case l of
            [] => (rev ac, [])
          | x :: l' =>
               if p x
                  then loop (l', x :: ac)
               else (rev ac, l)
   in loop (l, [])
   end

fun partition (l, p) =
   fold
   (l, {no = [], yes = []}, fn (x, {no, yes}) =>
    if p x
       then {no = no, yes = x :: yes}
    else {no = x :: no, yes = yes})

fun equivalence (l, p) =
   fold
   (l, [], fn (x, ecs) =>
    let
      fun loop ([], ecs') = [x]::ecs'
        | loop (ec::ecs, ecs') =
         if p (x, hd ec)
            then fold (ecs, (x::ec)::ecs', op ::)
         else loop (ecs, ec::ecs')
    in
       loop (ecs, [])
    end)
end
