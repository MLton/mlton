(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature UNCURRY_STRUCTS = 
   sig
      structure	Sxml: SXML
   end

signature UNCURRY = 
   sig
      include UNCURRY_STRUCTS
	 
      val uncurry: Sxml.Program.t -> Sxml.Program.t
   end
