(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Required functors *)

(* Optional functors *)
functor PrimIO (S: PRIM_IO_ARG): PRIM_IO = PrimIO (S)
functor StreamIO (S: STREAM_IO_ARG): STREAM_IO = StreamIO (S)
functor ImperativeIO (S: IMPERATIVE_IO_ARG): IMPERATIVE_IO = ImperativeIO (S)
