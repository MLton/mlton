(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Real64 = Real
structure Real32 = Real

(* Dummy implementation that will not be used at run-time. *)
structure PackReal32Big : PACK_REAL where type real = Real32.real = struct
   type real = Real32.real
   val bytesPerElem = 0
   val isBigEndian = false
   fun toBytes _ = raise Fail "PackReal32Big.toBytes"
   fun fromBytes _ = raise Fail "PackReal32Big.fromBytes"
   fun subVec _ = raise Fail "PackReal32Big.subVec"
   fun subArr _ = raise Fail "PackReal32Big.subArr"
   fun update _ = raise Fail "PackReal32Big.update"
end
(* Dummy implementation that will not be used at run-time. *)
structure PackReal32Little : PACK_REAL where type real = Real32.real = struct
   type real = Real32.real
   val bytesPerElem = 0
   val isBigEndian = false
   fun toBytes _ = raise Fail "PackReal32Little.toBytes"
   fun fromBytes _ = raise Fail "PackReal32Little.fromBytes"
   fun subVec _ = raise Fail "PackReal32Little.subVec"
   fun subArr _ = raise Fail "PackReal32Little.subArr"
   fun update _ = raise Fail "PackReal32Little.update"
end

(* Dummy implementation that will not be used at run-time. *)
structure PackReal64Big : PACK_REAL where type real = Real64.real  = struct
   type real = Real64.real
   val bytesPerElem = 0
   val isBigEndian = false
   fun toBytes _ = raise Fail "PackReal64Big.toBytes"
   fun fromBytes _ = raise Fail "PackReal64Big.fromBytes"
   fun subVec _ = raise Fail "PackReal64Big.subVec"
   fun subArr _ = raise Fail "PackReal64Big.subArr"
   fun update _ = raise Fail "PackReal64Big.update"
end
(* Dummy implementation that will not be used at run-time. *)
structure PackReal64Little : PACK_REAL where type real = Real64.real  = struct
   type real = Real64.real
   val bytesPerElem = 0
   val isBigEndian = false
   fun toBytes _ = raise Fail "PackReal64Little.toBytes"
   fun fromBytes _ = raise Fail "PackReal64Little.fromBytes"
   fun subVec _ = raise Fail "PackReal64Little.subVec"
   fun subArr _ = raise Fail "PackReal64Little.subArr"
   fun update _ = raise Fail "PackReal64Little.update"
end
