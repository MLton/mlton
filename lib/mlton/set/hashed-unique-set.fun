(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor HashedUniqueSet(structure Set : SET
                        structure Element : sig include T val hash : t -> word end
                        sharing type Set.Element.t = Element.t) : SET =
struct

structure Set = Set
structure Element = Element
val hash = Element.hash

fun index (w: word, mask: word): int
  = Word.toInt (Word.andb (w, mask))

datatype t = T of {buckets: Set.t vector,
                   mask: word} ref

fun stats' {buckets, mask}
  = Vector.fold
    (buckets,
     (0, NONE, NONE),
     fn (s', (size, min, max)) => let 
                                    val n = Set.size s'
                                  in
                                    (size + n,
                                     SOME (Option.fold(min,n,Int.min)),
                                     SOME (Option.fold(max,n,Int.max)))
                                  end)
fun stats s 
  = let
      val T (ref {buckets, mask}) = s
    in
      stats' {buckets = buckets, mask = mask}
    end

fun grow {buckets, mask}
  = let
      val mask' = mask
      val mask = Word.orb (0wx1, Word.<<(mask, 0wx1))
      val high = Word.andb (mask, Word.notb mask')

      val n = Vector.length buckets

      val buckets
        = (#1 o Vector.unfoldi)
          (2 * n,
           ([], false),
           fn (i, (l, b))
            => if b
                 then case l
                        of h::t => (h, (t, b))
                         | _ => Error.bug "HashedUniqueSet.grow"
                 else if i = n
                        then case List.rev l
                               of h::t => (h, (t, true))
                                | _ => Error.bug "HashedUniqueSet.grow"
                        else let
                               val {yes, no}
                                 = Set.partition
                                   (Vector.sub(buckets, i),
                                    fn x => Word.andb(high, hash x) = 0wx0)
                             in
                               (yes, (no::l, b))
                             end)
    in
      {buckets = buckets, mask = mask}
    end

fun shrink {buckets, mask}
  = let
      val mask = Word.>>(mask, 0wx1)

      val n = (Vector.length buckets) div 2

      val buckets
        = (#1 o Vector.unfoldi)
          (n,
           (),
           fn (i, _) => let
                          val s1 = Vector.sub(buckets, i)
                          val s2 = Vector.sub(buckets, i + n)
                        in
                          (Set.+(s1, s2), ())
                        end)
    in
      {buckets = buckets, mask = mask}
    end

fun T' {buckets, mask}
  = let
      val (size,min,max) = stats' {buckets = buckets, mask = mask}
      val max = case max of SOME max => max | NONE => ~1
      val n = Vector.length buckets
    in
      if max > n
        then T (ref (grow {buckets = buckets, mask = mask}))
      else if max < n div 2 andalso n > 2
        then T (ref (shrink {buckets = buckets, mask = mask}))
      else T (ref {buckets = buckets, mask = mask})
    end

fun coerce (s1 as T (s1' as ref (s1'' as {buckets = buckets1, mask = mask1})),
            s2 as T (s2' as ref (s2'' as {buckets = buckets2, mask = mask2})))
  = if mask1 = mask2
      then ()
      else if mask1 < mask2
             then (s1' := grow s1'';
                   coerce (s1, s2))
             else (s2' := grow s2'';
                   coerce (s1, s2))


val empty 
  = let
      val mask = 0wx1
      val buckets = Vector.new2 (Set.empty, Set.empty)
    in 
      T (ref {buckets = buckets,
              mask = mask})
    end
fun singleton x
  = let
      val mask = 0wx1
      val buckets
        = if Word.andb(mask, hash x) = 0wx0
            then Vector.new2 (Set.singleton x, Set.empty)
            else Vector.new2 (Set.empty, Set.singleton x)
    in
      T (ref {buckets = buckets,
              mask = mask})
    end


fun walk1 (vw, sw) s
  = let
      val T (ref {buckets, mask}) = s
    in
      vw(buckets, fn s' => sw s')
    end
fun walk2 (vw, sw) (s1, s2)
  = let
      val _ = coerce (s1, s2)
      val T (ref {buckets = buckets1, mask}) = s1
      val T (ref {buckets = buckets2, mask}) = s2
    in
      vw(buckets1, buckets2, fn (s1', s2') => sw (s1', s2'))
    end

val areDisjoint = walk2 (Vector.forall2, Set.areDisjoint)
val equals = walk2 (Vector.forall2, Set.equals)
fun exists (s, p) = walk1 (Vector.exists, fn s' => Set.exists(s', p)) s
fun forall (s, p) = walk1 (Vector.forall, fn s' => Set.forall(s', p)) s
fun foreach (s, f) = walk1 (Vector.foreach, fn s' => Set.foreach(s', f)) s

fun build1 sb s
  = let
      val T (ref {buckets, mask}) = s

      val buckets
        = (#1 o Vector.unfoldi)
          (Vector.length buckets,
           (),
           fn (i, _) => let
                          val s' = Vector.sub(buckets, i)
                        in
                          (sb s', ())
                        end)
    in
      T' {buckets = buckets, mask = mask}
    end
fun build2 sb (s1, s2)
  = let
      val _ = coerce (s1, s2)
      val T (ref {buckets = buckets1, mask}) = s1
      val T (ref {buckets = buckets2, mask}) = s2

      val buckets
        = (#1 o Vector.unfoldi)
          (Vector.length buckets1,
           (),
           fn (i, _) => let
                          val s1' = Vector.sub(buckets1, i)
                          val s2' = Vector.sub(buckets2, i)
                        in
                          (sb(s1', s2'), ())
                        end)
    in
      T' {buckets = buckets, mask = mask}
    end

val difference = build2 Set.-
val intersect = build2 Set.intersect
fun subset (s, p) = build1 (fn s' => Set.subset(s', p)) s
val union = build2 Set.+
fun unions [] = empty
  | unions [s] = s
  | unions [s1,s2] = union(s1, s2)
  | unions (s1::s2::ss) = unions(union(s1,s2)::ss)


fun contains (s, x)
  = let
      val T (ref {buckets, mask}) = s 
    in 
      Set.contains(Vector.sub(buckets, index(hash x, mask)), x)
    end
fun add (s, x)
  = if contains(s, x)
      then s
      else let
             val T (ref {buckets, mask}) = s
             val ix = index(hash x, mask)
             val buckets
               = (#1 o Vector.unfoldi)
                 (Vector.length buckets,
                  (),
                  fn (i, _) 
                   => let
                        val s' = Vector.sub(buckets, i)
                      in
                        if i = ix
                          then (Set.add(s', x), ())
                          else (s', ())
                      end)
           in 
             T' {buckets = buckets,
                 mask = mask}
           end
fun remove (s, x)
  = if not (contains(s, x))
      then s
      else let
             val T (ref {buckets, mask}) = s
             val ix = index(hash x, mask)
             val buckets
               = (#1 o Vector.unfoldi)
                 (Vector.length buckets,
                  (),
                  fn (i, _) 
                   => let
                        val s' = Vector.sub(buckets, i)
                      in
                        if i = ix
                          then (Set.remove(s', x), ())
                          else (s', ())
                      end)
           in 
             T' {buckets = buckets,
                 mask = mask}
           end
fun partition (s, p)
  = let
      val T (ref {buckets, mask}) = s 
      val n = Vector.length buckets
      val {yes, no}
        = Vector.fold
          (buckets,
           {yes = [], no = []},
           fn (s', {yes, no})
            => let
                 val {yes = yes', no = no'} = Set.partition (s', p)
               in
                 {yes = yes'::yes,
                  no = no'::no}
               end)
      val yes 
        = (#1 o Vector.unfoldi)
          (n,
           List.rev yes,
           fn (_, l) => case l
                          of h::t => (h, t)
                           | _ => Error.bug "HashedUniqueSet.partition.yes")
      val no
        = (#1 o Vector.unfoldi)
          (n,
           List.rev no,
           fn (_, l) => case l
                          of h::t => (h, t)
                           | _ => Error.bug "HashedUniqueSet.partition.no")
    in
      {yes = T' {buckets = yes, mask = mask},
       no = T' {buckets = no, mask = mask}}
    end


fun fold (s, b, f)
  = let
      val T (ref {buckets, mask}) = s 
    in
      Vector.fold
      (buckets,
       b,
       fn (s', b) => Set.fold(s', b, f))
    end

fun fromList l = List.fold(l, empty, fn (x, s) => add(s, x))
fun toList s = fold(s, [], op ::)
fun map (s, f) = fold(s, empty, fn (x, s) => add(s, f x))
fun replace (s, f)
  = fold(s, empty, fn (x, s) => case f x
                                  of NONE => s
                                   | SOME x' => add(s, x'))
fun subsetSize (s, p) 
  = fold(s, 0: int, fn (x, n) => if p x then n + 1 else n)
fun size s = subsetSize(s, fn _ => true)


fun layout s = List.layout Element.layout (toList s)

fun power s = Error.bug "HashedUniqueSet.power"
fun subsets (s, n) = Error.bug "HashedUniqueSet.subsets"

fun isEmpty s = size s = 0
fun isSubsetEq (s1, s2) = size (difference (s1, s2)) = 0
fun isSubset (s1, s2) = (size s1 <> size s2) andalso isSubsetEq(s1, s2)
fun isSupersetEq (s1, s2) = isSubsetEq(s2, s1)
fun isSuperset (s1, s2) = isSubset(s2, s1)

val op + = union
val op - = difference
val op < = isSubset
val op <= = isSubsetEq
val op > = isSuperset
val op >= = isSupersetEq

end
