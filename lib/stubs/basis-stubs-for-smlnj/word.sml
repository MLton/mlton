(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor FixWord(PWord: sig include WORD end) : WORD =
   struct
      open PWord

      local
         (* SML/NJ uses lower instead of upper case. *)
         val toUpper = String.translate (Char.toString o Char.toUpper)
      in
         fun fmt r w = toUpper (PWord.fmt r w)
         fun toString w = toUpper (PWord.toString w)
      end
   end

structure LargeWord = FixWord(struct open Pervasive.LargeWord end)
structure Word = FixWord(struct open Pervasive.Word end)
structure Word8 = FixWord(struct open Pervasive.Word8 end)
structure Word31 = FixWord(struct open Pervasive.Word31 end)
structure Word32 = FixWord(struct open Pervasive.Word32 end)
structure Word64 = FixWord(struct open Pervasive.Word64 end)
structure SysWord = FixWord(struct open Pervasive.SysWord end)

(* Dummy implementation that will not be used at run-time. *)
structure PackWord64Big : PACK_WORD = struct
   val bytesPerElem = 0
   val isBigEndian = true
   fun subVec _ = raise Fail "PackWord64Big.subVec"
   fun subVecX _ = raise Fail "PackWord64Big.subVecX"
   fun subArr _ = raise Fail "PackWord64Big.subArr"
   fun subArrX _ = raise Fail "PackWord64Big.subArrX"
   fun update _ = raise Fail "PackWord64Big.update"
end
(* Dummy implementation that will not be used at run-time. *)
structure PackWord64Little : PACK_WORD = struct
   val bytesPerElem = 0
   val isBigEndian = false
   fun subVec _ = raise Fail "PackWord64Little.subVec"
   fun subVecX _ = raise Fail "PackWord64Little.subVecX"
   fun subArr _ = raise Fail "PackWord64Little.subArr"
   fun subArrX _ = raise Fail "PackWord64Little.subArrX"
   fun update _ = raise Fail "PackWord64Little.update"
end
