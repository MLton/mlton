(* Copyright (C) 2018 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* simple (non-cryptographic) hashing methods *)
structure Hash: HASH =
struct
   (* Fowler–Noll–Vo hash,
    * but we use a different offset_basis that will fit
    * into a 31-bit word. It seems to have relatively
    * little impact. *)
   val prime = 0w16777619
   val offset_basis = 0wx55555 (* pretty much arbitrary, just shouldn't be zero *)
   fun multPrime n = prime * n
   fun combine (h1, h2) = multPrime (Word.xorb (h1, h2 + offset_basis))
   fun combine3 (h1, h2, h3) = combine (h1, combine (h2, h3))
   fun combine4 (h1, h2, h3, h4) = combine (h1, combine3 (h2, h3, h4))

   fun combineNoOffset (h1, h2) = multPrime (Word.xorb (h1, h2))
   fun vector vec = Vector.fold (vec, offset_basis, combineNoOffset)
   fun vectorMap (vec, f) = Vector.fold (vec, offset_basis, fn (v, k) => combineNoOffset (f v, k))
   fun list hs = List.fold (hs, offset_basis, combineNoOffset)
   fun listMap (hs, f) = List.fold (hs, offset_basis, fn (v, k) => combineNoOffset (f v, k))

   fun permute h = offset_basis + multPrime h
end
