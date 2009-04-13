(* memalloc.sig
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(* memalloc.sig
 *
 *   Primitives for "raw" memory allocation.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
signature CMEMALLOC = sig

    exception OutOfMemory

    eqtype addr'                (* to avoid clash with addr from CMEMACCESS *)

    val alloc : word -> addr'   (* may raise OutOfMemory *)
    val free : addr' -> unit
end
