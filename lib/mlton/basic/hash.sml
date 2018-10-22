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
   fun multPrime n = Word.<< (n, 0w24) + Word.<< (n, 0w8) + 0wx93 * n
   fun combineHelp hs k =
      case hs of
           [] => offset_basis
         | (h :: hs') => combineHelp hs' (multPrime (Word.xorb (h, k)))
   fun combine hs = combineHelp hs offset_basis
   fun permute h = offset_basis + multPrime h
end
