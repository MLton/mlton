(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature LOCAL_FLATTEN_STRUCTS = 
   sig
      include SHRINK
   end

signature LOCAL_FLATTEN = 
   sig
      include LOCAL_FLATTEN_STRUCTS

      (* Intraprocedural flattening. *)
      val flatten: Program.t -> Program.t
   end


functor TestLocalFlatten (S: LOCAL_FLATTEN): sig end = 
struct

val _ = print "TestLocalFlatten\n"

open S

val _ = Assert.assert ("LocalFlatten", fn () => true)

end
