(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Shrink (S: SHRINK_STRUCTS): SHRINK = 
struct

open S

fun shrinkFunction _ f = f

fun shrink p = p
   
end
