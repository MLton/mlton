(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor TextIOConvert
        (structure TextIO: TEXT_IO) :
        TEXT_IO_1997 =
  struct
     open TextIO

     fun inputLine ins =
        case TextIO.inputLine ins of
           NONE => ""
         | SOME s => s

     structure StreamIO =
        struct
           open StreamIO

           val inputAll = #1 o inputAll

           fun inputLine ins =
              case StreamIO.inputLine ins of
                 NONE => ("", ins)
               | SOME (s, ins) => (s, ins)
        end
  end
