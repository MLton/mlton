(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

structure Primitive = struct

open Primitive

structure PackReal32 =
   struct
      type real = Real32.real

      val subArr = 
         _import "PackReal32_subArr": Word8.word array * C_Ptrdiff.t -> real;
      val subArrRev = 
         _import "PackReal32_subArrRev": Word8.word array * C_Ptrdiff.t -> real;
      val subVec = 
         _import "PackReal32_subVec": Word8.word vector * C_Ptrdiff.t -> real;
      val subVecRev = 
         _import "PackReal32_subVecRev": Word8.word vector * C_Ptrdiff.t -> real;
      val update = 
         _import "PackReal32_update": Word8.word array * C_Ptrdiff.t * real -> unit;
      val updateRev = 
         _import "PackReal32_updateRev": Word8.word array * C_Ptrdiff.t * real -> unit;
   end

structure PackReal64 =
   struct
      type real = Real64.real

      val subArr = 
         _import "PackReal64_subArr": Word8.word array * C_Ptrdiff.t -> real;
      val subArrRev = 
         _import "PackReal64_subArrRev": Word8.word array * C_Ptrdiff.t -> real;
      val subVec = 
         _import "PackReal64_subVec": Word8.word vector * C_Ptrdiff.t -> real;
      val subVecRev =
         _import "PackReal64_subVecRev": Word8.word vector * C_Ptrdiff.t -> real;
      val update =
         _import "PackReal64_update": Word8.word array * C_Ptrdiff.t * real -> unit;
      val updateRev =
         _import "PackReal64_updateRev": Word8.word array * C_Ptrdiff.t * real -> unit;
   end

end
