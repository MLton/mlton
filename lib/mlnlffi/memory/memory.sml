(* memory.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(* memory.sml
 *
 *   Primitives for "raw" memory access and allocation.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure CMemory : CMEMORY = struct
    open CMemAccess
    open CMemAlloc
end
