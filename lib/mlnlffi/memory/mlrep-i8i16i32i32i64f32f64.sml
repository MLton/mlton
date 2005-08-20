(* mlrep-i8i16i32i32i64f32f64.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *    char      : Int8, Word8
 *    short     : Int16, Word16
 *    int       : Int32, Word32
 *    long      : Int32, Word32
 *    long long : Int64, Word64
 *    float     : Real32
 *    double    : Real64
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
          structure Signed = Int8
          structure Unsigned = Word8
          (* word-style bit-operations on integers... *)
          structure SignedBitops = IntBitOps(structure I = Signed
                                             structure W = Unsigned)
       end
    structure Short =
       struct
          structure Signed = Int16
          structure Unsigned = Word16
          (* word-style bit-operations on integers... *)
          structure SignedBitops = IntBitOps(structure I = Signed
                                             structure W = Unsigned)
       end
    structure Int =
       struct
          structure Signed = Int32
          structure Unsigned = Word32
          (* word-style bit-operations on integers... *)
          structure SignedBitops = IntBitOps(structure I = Signed
                                             structure W = Unsigned)
       end  
    structure Long =
       struct
          structure Signed = Int32
          structure Unsigned = Word32
          (* word-style bit-operations on integers... *)
          structure SignedBitops = IntBitOps(structure I = Signed
                                             structure W = Unsigned)
       end
    structure LongLong =
       struct
          structure Signed = Int64
          structure Unsigned = Word64
          (* word-style bit-operations on integers... *)
          structure SignedBitops = IntBitOps(structure I = Signed
                                             structure W = Unsigned)
       end
    structure Float = Real32
    structure Double = Real64
end
