(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor UniqueId() :> UNIQUE_ID =
   struct
      type id = unit ref

      fun new(): id = ref()

      val equals = op =
   end
