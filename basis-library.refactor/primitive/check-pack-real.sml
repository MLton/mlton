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
      structure PR1 = Primitive.PackReal32
      structure PR2 = PrimitiveFFI.PackReal32
   in
      val () = check (PR1.subArr, PR2.subArr)
      val () = check (PR1.subArrRev, PR2.subArrRev)
      val () = check (PR1.subVec, PR2.subVec)
      val () = check (PR1.subVecRev, PR2.subVecRev)
      val () = check (PR1.update, PR2.update)
      val () = check (PR1.updateRev, PR2.updateRev)
   end

   local
      structure PR1 = Primitive.PackReal64
      structure PR2 = PrimitiveFFI.PackReal64
   in
      val () = check (PR1.subArr, PR2.subArr)
      val () = check (PR1.subArrRev, PR2.subArrRev)
      val () = check (PR1.subVec, PR2.subVec)
      val () = check (PR1.subVecRev, PR2.subVecRev)
      val () = check (PR1.update, PR2.update)
      val () = check (PR1.updateRev, PR2.updateRev)
   end
in

end
