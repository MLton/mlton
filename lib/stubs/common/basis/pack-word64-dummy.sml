(* Copyright (C) 2009,2022 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Dummy implementation that will not be used at run-time. *)
structure PackWord64Little : PACK_WORD = struct
   val bytesPerElem = 8
   val isBigEndian = false
   fun subVec _ = raise Fail "PackWord64Little.subVec"
   fun subVecX _ = raise Fail "PackWord64Little.subVecX"
   fun subArr _ = raise Fail "PackWord64Little.subArr"
   fun subArrX _ = raise Fail "PackWord64Little.subArrX"
   fun update _ = raise Fail "PackWord64Little.update"
end
