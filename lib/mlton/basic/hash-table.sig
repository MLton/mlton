(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* This code is not working -- it is not even in sources.cm *)
signature HASH_TABLE =
   sig
      type ('a, 'b) t

      val fold: ('a, 'b) t * 'c * ('b * 'c -> 'c) -> 'c
      val foldi: ('a, 'b) t * 'c * ('a * 'b * 'c -> 'c) -> 'c
      val forall: ('a, 'b) t * ('b -> bool) -> bool
      val foralli: ('a, 'b) t * ('a * 'b -> bool) -> bool
      val foreach: ('a, 'b) t * ('b -> unit) -> unit
      val foreachi: ('a, 'b) t * ('a * 'b -> unit) -> unit
      (* If it's already in the table, call the thunk, else insert it and
       * return it.
       *)
      val insertIfNew: ('a, 'b) t * 'a * 'b * (unit -> 'b) -> 'b
      val listItems: ('a, 'b) t -> 'b list
      val listItemsi: ('a, 'b) t -> ('a * 'b) list
      val layout: ('a * 'b -> Layout.t) -> ('a, 'b) t -> Layout.t
      val lookupOrInsert: ('a, 'b) t * 'a * (unit -> 'b) -> 'b
      val map: ('a, 'b) t * ('b -> 'c) -> ('a, 'c) t
      val mapi: ('a, 'b) t * ('a * 'b -> 'c) -> ('a, 'c) t
      val new: {equals: 'a * 'a -> bool,
                hash: 'a -> word} -> ('a, 'b) t
      val peek: ('a, 'b) t * 'a -> 'b option
      val size: ('a, 'b) t -> int
      val stats: unit -> Layout.t
      val update: ('a, 'b) t * 'a * 'b -> unit
   end

functor TestHashTable (S: HASH_TABLE): sig end =
struct

open S

val _ =
   Assert.assert
   ("TestHashTable", fn () => 
    let val t = new Int.equals
       val n = 10
       val hash = Word.fromInt
       val _ =
          Int.for(0, n, fn i =>
                  (lookupOrInsert(t, hash i, i, fn () => i * 2)
                   ; ()))
       val sum = Int.fold(0, n, 0, op +)
    in
       let val r = ref 0
       in foreach (t, fn j => r := !r + j)
          ; 2 * sum = !r
       end
    andalso Int.forall(0, n, fn i => Option.isSome(peek(t, hash i, i)))
    andalso foralli(t, fn (i, j) => j = 2 * i)
    andalso n = List.length(listItems t)
    andalso n = List.length(listItemsi t)
    andalso let val t' = map(t, fn j => j div 2)
            in n = size t'
               andalso foralli(t', fn (i, j) => i = j)
            end
         andalso n = size t
    end)

end
