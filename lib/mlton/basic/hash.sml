(* Copyright (C) 2017 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* simple (non-cryptographic) hashing methods *)
structure Hash: HASH =
struct
   (* Fowler–Noll–Vo hash *)
   val prime = 0w16777619
   val offset_basis = 0w2166136261
   fun multPrime n = prime * n
   fun combine2 (h, k) = multPrime (Word.xorb (h, k))
   fun combine hs = List.fold (hs, offset_basis, combine2)
   fun combineVec vec = Vector.fold (vec, offset_basis, combine2)
   fun permute h = offset_basis + multPrime h
end
