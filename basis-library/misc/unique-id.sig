(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature UNIQUE_ID =
   sig
      type id
      val new: unit -> id
      val equals: id * id -> bool
   end
