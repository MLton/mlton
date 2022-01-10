(* Copyright (C) 2009,2022 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Dummy implementation that will not be used at run-time. *)
structure PackReal64Little : PACK_REAL where type real = Real64.real  = struct
   type real = Real64.real
   val bytesPerElem = 8
   val isBigEndian = false
   fun toBytes _ = raise Fail "PackReal64Little.toBytes"
   fun fromBytes _ = raise Fail "PackReal64Little.fromBytes"
   fun subVec _ = raise Fail "PackReal64Little.subVec"
   fun subArr _ = raise Fail "PackReal64Little.subArr"
   fun update _ = raise Fail "PackReal64Little.update"
end
