(* Copyright (C) 2019 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Rssa (S: RSSA_STRUCTS): RSSA =
   RssaSimplify (RssaRestore (RssaLive (RssaTree (S))))
