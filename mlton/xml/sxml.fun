(* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Sxml (S: SXML_STRUCTS): SXML =
  SxmlSimplify (struct
                   open S
                   structure Program =
                      struct
                         open Program
                         val layoutStats =
                            mkLayoutStats "sxml"
                         val toFile =
                            {display = #display toFile,
                             style = #style toFile,
                             suffix = "sxml"}
                      end
                end)
