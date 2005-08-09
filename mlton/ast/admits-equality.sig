(* Copyright (C) 2003-2003 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature ADMITS_EQUALITY_STRUCTS = 
   sig
   end

signature ADMITS_EQUALITY = 
   sig
      include ADMITS_EQUALITY_STRUCTS
      
      datatype t = Always | Never | Sometimes

      val <= : t * t -> bool
      val layout: t -> Layout.t
      val or: t * t -> t
      val toString: t -> string
   end
