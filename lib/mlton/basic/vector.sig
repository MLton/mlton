(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature VECTOR_STRUCTS =
   sig
      type 'a t
      exception New

      val length: 'a t -> int
      val sub: 'a t * int -> 'a
      val unfoldi: int * 'b * (int * 'b -> 'a * 'b) -> 'a t * 'b
   end

signature VECTOR =
   sig
      include VECTOR_STRUCTS

      val compare: 'a t * 'a t * ('a * 'a -> order) -> order
      val concat: 'a t list -> 'a t
      val concatV: 'a t t -> 'a t
      val contains: 'a t * 'a * ('a * 'a -> bool) -> bool
      val copy: 'a t -> 'a t
      val dropPrefix: 'a t * int -> 'a t
      val dropSuffix: 'a t * int -> 'a t
      val equals: 'a t * 'b t * ('a * 'b -> bool) -> bool
      val exists: 'a t * ('a -> bool) -> bool
      val existsi: 'a t * (int * 'a -> bool) -> bool
      val existsR: 'a t * int * int * ('a -> bool) -> bool
      val fold2: 'a t * 'b t * 'c * ('a * 'b * 'c -> 'c) -> 'c
      val fold3From:
         'a t * 'b t * 'c t * int * 'd * ('a * 'b * 'c * 'd -> 'd) -> 'd
      val fold3: 'a t * 'b t * 'c t * 'd * ('a * 'b * 'c * 'd -> 'd) -> 'd
      datatype ('a, 'b) continue =
         Continue of 'a
       | Done of 'b
      (* fold' (v, i, b, f, g)
       * folds over v starting at index i with state b, applying f to each
       * index, vector element, and state, continuing depending on what f
       * returns.  If the end of the vector is reached, g is applied to the
       * state.
       *)

      val first: 'a t -> 'a
      val fold':
         'a t * int * 'b * (int * 'a * 'b -> ('b, 'c) continue) * ('b -> 'c)
         -> 'c
      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foldFrom: 'a t * int * 'b * ('a * 'b -> 'b) -> 'b
      val foldi: 'a t * 'b * (int * 'a * 'b -> 'b) -> 'b
      val foldi2From: 'a t * 'b t * int * 'c * (int * 'a * 'b * 'c -> 'c) -> 'c
      val foldr: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foldri: 'a t * 'b * (int * 'a * 'b -> 'b) -> 'b
      val foldr2: 'a t * 'b t * 'c * ('a * 'b * 'c -> 'c) -> 'c
      val forall: 'a t * ('a -> bool) -> bool
      val forall2: 'a t * 'b t * ('a * 'b -> bool) -> bool
      val foralli: 'a t * (int * 'a -> bool) -> bool
      val foreach: 'a t * ('a -> unit) -> unit
      val foreachi: 'a t * (int * 'a -> unit) -> unit
      val foreachi2: 'a t * 'b t * (int * 'a * 'b -> unit) -> unit
      val foreachr: 'a t * ('a -> unit) -> unit
      val foreachri: 'a t * (int * 'a -> unit) -> unit
      val foreach2: 'a t * 'b t * ('a * 'b -> unit) -> unit
      val foreachR: 'a t * int * int * ('a -> unit) -> unit
      val foreach3: 'a t * 'b t * 'c t * ('a * 'b * 'c -> unit) -> unit
      val fromArray: 'a array -> 'a t
      val fromList: 'a list -> 'a t
      val fromListMap: 'a list * ('a -> 'b) -> 'b t
      val fromListRev: 'a list -> 'a t
      val indexi: 'a t * (int * 'a -> bool) -> int option
      val index: 'a t * ('a -> bool) -> int option
      val indices: bool t -> int t
      val isEmpty: 'a t -> bool
      val isSorted: 'a t * ('a * 'a -> bool) -> bool
      (* isSortedRange (v, l, u, <=) checks if [l, u) is sorted. *)
      val isSortedRange: 'a t * int * int * ('a * 'a -> bool) -> bool
      val isSubsequence: 'a t * 'b t * ('a * 'b -> bool) -> bool
      val keepAll: 'a t * ('a -> bool) -> 'a t
      val keepAllMap: 'a t * ('a -> 'b option) -> 'b t
      val keepAllMapi: 'a t * (int * 'a -> 'b option) -> 'b t
      val keepAllMap2: 'a t * 'b t * ('a * 'b -> 'c option) -> 'c t
      val keepAllSome: 'a option t -> 'a t
      val last: 'a t -> 'a
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val loop: 'a t * ('a -> 'b option) * (unit -> 'b) -> 'b
      val loopi: 'a t * (int * 'a -> 'b option) * (unit -> 'b) -> 'b
      val map: 'a t * ('a -> 'b) -> 'b t
      val map2: 'a t * 'b t * ('a * 'b -> 'c) -> 'c t
      val map3: 'a t * 'b t * 'c t * ('a * 'b * 'c -> 'd) -> 'd t
      val mapAndFold: 'a t * 'b * ('a * 'b -> 'c * 'b) -> 'c t * 'b
      val map2AndFold: 'a t * 'b t * 'c * ('a * 'b * 'c -> 'd * 'c) -> 'd t * 'c
      val mapi: 'a t * (int * 'a -> 'b) -> 'b t
      val new: int * 'a -> 'a t
      val new0: unit -> 'a t
      val new1: 'a -> 'a t
      val new2: 'a * 'a -> 'a t
      val new3: 'a * 'a * 'a -> 'a t
      val new4: 'a * 'a * 'a * 'a -> 'a t
      val new5: 'a * 'a * 'a * 'a * 'a -> 'a t
      val new6: 'a * 'a * 'a * 'a * 'a * 'a -> 'a t
      val partition: 'a t * ('a -> bool) -> {no: 'a t, yes: 'a t}
      val partitioni: 'a t * (int * 'a -> bool) -> {no: 'a t, yes: 'a t}
      val peek: 'a t * ('a -> bool) -> 'a option
      val peeki: 'a t * (int * 'a -> bool) -> (int * 'a) option
      val peekMap: 'a t * ('a -> 'b option) -> 'b option
      val peekMapi: 'a t * ('a -> 'b option) -> (int * 'b) option
      val prefix: 'a t * int -> 'a t
      val randomElement: 'a t -> 'a
      val removeDuplicates: 'a t * ('a * 'a -> bool) -> 'a t
      val removeFirst: 'a t * ('a -> bool) -> 'a t
      val rev: 'a t -> 'a t
      val size: 'a t -> int
      val splitLast: 'a t -> 'a t * 'a
      val tabulate: int * (int -> 'a) -> 'a t
      val tabulator: int * (('a -> unit) -> unit) -> 'a t
      val toArray: 'a t -> 'a array
      val toList: 'a t -> 'a list
      val toListMap: 'a t * ('a -> 'b) -> 'b list
      val toListRev: 'a t -> 'a list
      val toString: ('a -> string) -> 'a t -> string
      val unzip: ('a * 'b) t -> 'a t * 'b t
      val unzip3: ('a * 'b * 'c) t -> 'a t * 'b t * 'c t
      val zip: 'a t * 'b t -> ('a * 'b) t
   end

functor TestVector (S: VECTOR): sig end =
struct

open S

val _ =
   Assert.assert
   ("TestVector", fn () =>
    let
       fun check ls =
          List.concat ls = toList (concat (List.map (ls, fromList)))
          andalso List.concat ls = toList (concatV (fromListMap (ls, fromList)))
    in
       List.forall
       ([[],
         [[]],
         [[], [1]],
         [[1], []],
         [[1], [], [2]],
         [[1, 2], [3, 4]]],
        check)
    end)

end
