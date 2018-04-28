(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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
