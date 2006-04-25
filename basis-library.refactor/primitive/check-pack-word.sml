(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

local
   fun 'a check (x: 'a, y: 'a) : unit = ()

   local
      structure PW1 = Primitive.PackWord8
      structure PW2 = PrimitiveFFI.PackWord8
   in
      val () = check (PW1.subArr, PW2.subArr)
      val () = check (PW1.subArrRev, PW2.subArrRev)
      val () = check (PW1.subVec, PW2.subVec)
      val () = check (PW1.subVecRev, PW2.subVecRev)
      val () = check (PW1.update, PW2.update)
      val () = check (PW1.updateRev, PW2.updateRev)
   end

   local
      structure PW1 = Primitive.PackWord16
      structure PW2 = PrimitiveFFI.PackWord16
   in
      val () = check (PW1.subArr, PW2.subArr)
      val () = check (PW1.subArrRev, PW2.subArrRev)
      val () = check (PW1.subVec, PW2.subVec)
      val () = check (PW1.subVecRev, PW2.subVecRev)
      val () = check (PW1.update, PW2.update)
      val () = check (PW1.updateRev, PW2.updateRev)
   end

   local
      structure PW1 = Primitive.PackWord32
      structure PW2 = PrimitiveFFI.PackWord32
   in
      val () = check (PW1.subArr, PW2.subArr)
      val () = check (PW1.subArrRev, PW2.subArrRev)
      val () = check (PW1.subVec, PW2.subVec)
      val () = check (PW1.subVecRev, PW2.subVecRev)
      val () = check (PW1.update, PW2.update)
      val () = check (PW1.updateRev, PW2.updateRev)
   end

   local
      structure PW1 = Primitive.PackWord64
      structure PW2 = PrimitiveFFI.PackWord64
   in
      val () = check (PW1.subArr, PW2.subArr)
      val () = check (PW1.subArrRev, PW2.subArrRev)
      val () = check (PW1.subVec, PW2.subVec)
      val () = check (PW1.subVecRev, PW2.subVecRev)
      val () = check (PW1.update, PW2.update)
      val () = check (PW1.updateRev, PW2.updateRev)
   end
in

end
