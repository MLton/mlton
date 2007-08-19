(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Bool: BOOL =
   struct
      datatype bool = datatype bool

      val not = not

      fun scan reader state =
         case reader state of
            NONE => NONE
          | SOME(c, state) =>
               case c of
                  #"f" => (case Reader.reader4 reader state of
                              SOME((#"a", #"l", #"s", #"e"), state) =>
                                 SOME(false, state)
                            | _ => NONE)
                | #"t" => (case Reader.reader3 reader state of
                              SOME((#"r", #"u", #"e"), state) =>
                                 SOME(true, state)
                            | _ => NONE)
                | _ => NONE

      val fromString = StringCvt.scanString scan

      val toString =
         fn true => "true"
          | false => "false"
   end

structure BoolGlobal: BOOL_GLOBAL = Bool
open BoolGlobal

