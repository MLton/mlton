(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature REMOVE_UNUSED_STRUCTS = 
   sig
      include SHRINK
   end

signature REMOVE_UNUSED = 
   sig
      include REMOVE_UNUSED_STRUCTS
      
      val remove: Program.t -> Program.t
   end


functor TestRemoveUnused(S: REMOVE_UNUSED) = 
struct

open S

val _ = Assert.assert("RemoveUnused", fn () => true)

end
