(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature CALL_COUNT_STRUCTS = 
   sig
      include XML
   end

signature CALL_COUNT = 
   sig
      include CALL_COUNT_STRUCTS

      (* Instrument the program so that the C primitive of the given name
       * is applied at each call.
       * For now, the string should either be Xml or Sxml.
       *)
      val instrument: Program.t * string -> Program.t
   end
