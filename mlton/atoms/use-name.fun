(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor UseName(S: sig
		       include T
		       val sameName: t * t -> bool
		    end): T =
   struct
      open S
      val equals = sameName
   end
