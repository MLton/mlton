(* Copyright (C) 2026 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Bool: BOOL =
   struct
      datatype bool = datatype bool

      val not = not

      fun scan reader s =
         let
            val s = StringCvt.skipWS reader s
            val reader = StringCvt.map Char.toLower reader
         in
            case reader s of
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
         end

      val fromString = StringCvt.scanString scan

      val toString =
         fn true => "true"
          | false => "false"
   end

structure BoolGlobal: BOOL_GLOBAL = Bool
open BoolGlobal
