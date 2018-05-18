(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor OrderedUniqueSet(Element : ORDER): SET = 
struct

structure Element = Element

datatype t = T of {elements: Element.t list,
                   length: int}

fun T' _ s
  = let
(*
      val _ 
        = Assert.assert
          ("OrderedUniqueSet, " ^ f,
           fn () => let
                      val rec check
                        = fn h1::(t as h2::_)
                           => if Element.compare(h1, h2) = LESS
                                then check t
                                else (print (concat
                                             [Layout.toString (Element.layout h1),
                                              ", ",
                                              Layout.toString (Element.layout h2),
                                              "\n",
                                              Layout.toString (Relation.layout (Element.compare(h1, h2))),
                                              "\n",
                                              Layout.toString (Relation.layout (Element.compare(h2, h1))),
                                              "\n"
                                              ]);
                                      false)
                           | _ => true
                    in 
                      (List.length elements = length)
                      andalso
                      check elements
                    end)
          handle exn => (print (Layout.toString (List.layout Element.layout elements));
                         print "\n";
                         raise exn)
*)
    in
      T s
    end

val empty' = {elements = [], length = 0: int}
val empty = T' "empty'" empty'
fun singleton x = T' "singleton" {elements = [x], length = 1}

fun contains (T {elements = xs, ...}, x)
  = let
      val rec contains' 
        = fn [] => false
           | h::t => if Element.<(h, x)
                       then contains' t
                     else if Element.>(h,x)
                            then false
                     else true
    in
      contains' xs
    end

fun add (s as T s', x)
  = let
      val rec add'
        = fn ({elements = [], ...},
              {elements = xsacc, length = nacc})
           => {elements = List.appendRev(xsacc, [x]), length = nacc + 1}
           | ({elements = xs as h::t, length = n},
              {elements = xsacc, length = nacc})
           => if Element.<(h,x)
                then add' ({elements = t, length = n - 1},
                           {elements = h::xsacc, length = 1 + nacc})
              else if Element.>(h,x)
                then {elements = List.appendRev(xsacc, x::xs),
                      length = nacc + 1 + n}
              else {elements = List.appendRev(xsacc, xs),
                    length = nacc + n}
    in
      if contains(s, x)
        then s
        else T' "add" (add' (s', empty'))
    end

fun areDisjoint (T {elements = xs1, ...}, T {elements = xs2, ...})
  = let
      val rec areDisjoint'
        = fn ([], _) => true
           | (_, []) => true
           | (xs1 as h1::t1, 
              xs2 as h2::t2)
           => if Element.<(h1, h2)
                then areDisjoint'(t1, xs2)
              else if Element.>(h1, h2)
                then areDisjoint'(xs1, t2)
              else false
    in
      areDisjoint' (xs1, xs2)
    end

fun difference (T s1', T s2')
  = let
      val rec difference'
        = fn ({elements = [], ...},
              _,
              {elements = xsacc, length = nacc})
           => {elements = List.rev xsacc, length = nacc}
           | ({elements = xs1, length = n1},
              {elements = [], ...},
              {elements = xsacc, length = nacc})
           => {elements = List.appendRev(xsacc, xs1), length = nacc + n1}
           | (s1 as {elements = h1::t1, length = n1},
              s2 as {elements = h2::t2, length = n2},
              sacc as {elements = xsacc, length = nacc})
           => if Element.<(h1,h2)
                then difference' ({elements = t1, length = n1 - 1},
                                  s2,
                                  {elements = h1::xsacc, length = 1 + nacc})
              else if Element.>(h1,h2)
                then difference' (s1,
                                  {elements = t2, length = n2 - 1},
                                  sacc)
              else difference' ({elements = t1, length = n1 - 1},
                                {elements = t2, length = n2 - 1},
                                sacc)
    in
      T' "difference" (difference' (s1', s2', empty'))
    end

fun equals (T {elements = xs1, length = n1},
            T {elements = xs2, length = n2})
  = let
      val rec equals' 
        = fn ([], []) => true
           | ([], _) => false
           | (_, []) => false
           | (h1::t1, h2::t2) => Element.equals(h1, h2)
                                 andalso
                                 equals'(t1, t2)
    in
      n1 = n2 
      andalso
      equals' (xs1, xs2)
    end

fun exists (T {elements = xs, ...}, p) = List.exists(xs, p)

fun fold (T {elements = xs, ...}, b, f) = List.fold(xs, b, f)

fun forall (T {elements = xs, ...}, p) = List.forall(xs, p)
fun foreach (T {elements = xs, ...}, p) = List.foreach(xs, p)

fun fromList l = List.fold(l, empty, fn (x, s) => add(s, x))

fun intersect (T s1', T s2')
  = let
      val rec intersect'
        = fn ({elements = [], ...},
              _,
              {elements = xsacc, length = nacc})
           => {elements = List.rev xsacc, length = nacc}
           | (_,
              {elements = [], ...},
              {elements = xsacc, length = nacc})
           => {elements = List.rev xsacc, length = nacc}
           | (s1 as {elements = h1::t1, length = n1},
              s2 as {elements = h2::t2, length = n2},
              sacc as {elements = xsacc, length = nacc})
           => if Element.<(h1,h2)
                then intersect' ({elements = t1, length = n1 - 1},
                                 s2,
                                 sacc)
              else if Element.>(h1,h2)
                then intersect' (s1,
                                 {elements = t2, length = n2 - 1},
                                 sacc)
              else intersect' ({elements = t1, length = n1 - 1},
                               {elements = t2, length = n2 - 1},
                               {elements = h1::xsacc, length = 1 + nacc})
    in
      T' "intersect" (intersect' (s1', s2', empty'))
    end

fun layout (T {elements = xs, ...}) = List.layout Element.layout xs
fun map (T {elements = xs, ...}, f) = fromList(List.map(xs, f))

fun partition (T {elements = xs, ...}, p)
  = let
      val {yes = {elements = yxs, length = yn},
           no = {elements = nxs, length = nn}}
        = List.fold(xs, 
                    {yes = empty',
                     no = empty'},
                    fn (x, 
                        {yes as {elements = yxs, length = yn},
                         no as {elements = nxs, length = nn}})
                     => if p x
                          then {yes = {elements = x::yxs, length = yn + 1},
                                no = no}
                          else {yes = yes,
                                no = {elements = x::nxs, length = nn + 1}})
    in
      {yes = T' "partition" {elements = List.rev yxs, length = yn},
       no = T' "partition" {elements = List.rev nxs, length = nn}}
    end

fun power (T {elements = xs, ...})
  = let
      val rec power'
        = fn [] => [empty]
           | h::t => let
                       val rest = power' t
                     in
                       List.fold
                       (rest,
                        rest,
                        fn (T {elements = xs, length = n}, rest) 
                         => (T' "power" {elements = h::xs, length = 1 + n})::rest)
                     end
    in
      power' xs
    end

fun remove (T s', x)
  = let
      val rec remove' 
        = fn ({elements = [], ...},
              {elements = xsacc, length = nacc})
           => {elements = List.appendRev(xsacc, [x]), length = nacc + 1}
           | ({elements = xs as h::t, length = n},
              {elements = xsacc, length = nacc})
           => if Element.<(h, x)
                then remove' ({elements = t, length = n - 1},
                              {elements = h::xsacc, length = 1 + nacc})
              else if Element.>(h, x)
                then {elements = List.appendRev(xsacc, xs),
                      length = nacc + n}
              else {elements = List.appendRev(xsacc, t),
                    length = nacc + n - 1}
    in
      T' "remove" (remove' (s', empty'))
    end

fun replace (T {elements = xs, ...}, f)
  = List.fold(xs, 
              empty, 
              fn (x, s) => (case f x
                              of NONE => s
                               | SOME x' => add(s, x')))

fun size (T {length = n, ...}) = n

fun subset (T {elements = xs, ...}, p)
  = let
      val {elements = xs, length = n}
        = List.fold(xs, 
                    empty',
                    fn (x, s as {elements = xs, length = n})
                     => if p x
                          then {elements = x::xs, length = n + 1}
                          else s)
    in
      T' "subset" {elements = List.rev xs, length = n}
    end

fun subsets _ = Error.unimplemented "OrderedUniqueSet: subsets"

fun subsetSize (T {elements = xs, ...}, p)
  = List.fold(xs, 0: int, fn (x, n) => if p x then n + 1 else n)

fun toList (T {elements = xs, ...}) = xs

fun union (T s1', T s2')
  = let
      val rec union' 
        = fn ({elements = [], ...},
              {elements = xs2, length = n2},
              {elements = xsacc, length = nacc})
           => {elements = List.appendRev(xsacc, xs2),
               length = nacc + n2}
           | ({elements = xs1, length = n1},
              {elements = [], ...},
              {elements = xsacc, length = nacc})
           => {elements = List.appendRev(xsacc, xs1),
               length = nacc + n1}
           | (s1 as {elements = h1::t1, length = n1},
              s2 as {elements = h2::t2, length = n2},
              {elements = xsacc, length = nacc})
           => if Element.<(h1,h2)
                then union' ({elements = t1, length = n1 - 1},
                             s2,
                             {elements = h1::xsacc, length = 1 + nacc})
              else if Element.>(h1,h2)
                then union' (s1,
                             {elements = t2, length = n2 - 1},
                             {elements = h2::xsacc, length = 1 + nacc})
              else union' ({elements = t1, length = n1 - 1},
                           {elements = t2, length = n2 - 1},
                           {elements = h1::xsacc, length = 1 + nacc})
    in
      T' "union" (union' (s1', s2', empty'))
    end

fun unions ss = List.fold(ss, empty, union)

fun isEmpty s = size s = 0
fun isSubsetEq (s1, s2) = size (difference (s1, s2)) = 0
fun isSubset (s1, s2) = (size s1 <> size s2) andalso isSubsetEq(s1, s1)
fun isSupersetEq (s1, s2) = isSubsetEq(s2, s1)
fun isSuperset (s1, s2) = isSubset(s2, s1)

val op + = union
val op - = difference
val op < = isSubset
val op <= = isSubsetEq
val op > = isSuperset
val op >= = isSupersetEq


end
