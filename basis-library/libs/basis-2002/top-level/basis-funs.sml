(* Copyright (C) 2002-2003 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

(* Required functors *)

(* Optional functors *)
functor PrimIO (S: PRIM_IO_ARG): PRIM_IO = PrimIO (S)
functor StreamIO (S: STREAM_IO_ARG): STREAM_IO = StreamIO (S)
functor ImperativeIO (S: IMPERATIVE_IO_ARG): IMPERATIVE_IO = ImperativeIO (S)
