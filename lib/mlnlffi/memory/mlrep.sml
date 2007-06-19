(* mlrep.sml
 * 2007 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.  Make use of $(SML_LIB)/basis/c-types.mlb
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(* mlrep-i32f64.sml
 *
 *   User-visible ML-side representation of certain primitive C types.
 *   x86/Sparc/PPC version (all ints: 32 bit, all floats: 64 bit)
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure MLRep = struct
    structure Char =
       struct
          structure Signed = C_SChar
          structure Unsigned = C_UChar
          (* word-style bit-operations on integers... *)
          structure SignedBitops = IntBitOps(structure I = Signed
                                             structure W = Unsigned)
       end
    structure Short =
       struct
          structure Signed = C_SShort
          structure Unsigned = C_UShort
          (* word-style bit-operations on integers... *)
          structure SignedBitops = IntBitOps(structure I = Signed
                                             structure W = Unsigned)
       end
    structure Int =
       struct
          structure Signed = C_SInt
          structure Unsigned = C_UInt
          (* word-style bit-operations on integers... *)
          structure SignedBitops = IntBitOps(structure I = Signed
                                             structure W = Unsigned)
       end  
    structure Long =
       struct
          structure Signed = C_SLong
          structure Unsigned = C_ULong
          (* word-style bit-operations on integers... *)
          structure SignedBitops = IntBitOps(structure I = Signed
                                             structure W = Unsigned)
       end
    structure LongLong =
       struct
          structure Signed = C_SLongLong
          structure Unsigned = C_ULongLong
          (* word-style bit-operations on integers... *)
          structure SignedBitops = IntBitOps(structure I = Signed
                                             structure W = Unsigned)
       end
    structure Float = C_Float
    structure Double = C_Double
end
