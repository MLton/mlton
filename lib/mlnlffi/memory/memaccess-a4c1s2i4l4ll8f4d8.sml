(* memaccess-a4c1s2i4l4ll8f4d8.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(* memaccess-64-big.sml *)
(* memaccess-64-little.sml *)
(* memaccess-a4s2i4l4f4d8.sml
 *
 *   Primitives for "raw" memory access.
 *
 *   x86/Sparc/PPC version:
 *       addr char short  int  long float double
 *       4    1    2      4    4    4     8       (bytes)
 *
 *   (C) 2004 The Fellowship of SML/NJ
 *
 * author: Matthias Blume (blume@tti-c.org)
 *)
structure CMemAccess : CMEMACCESS = struct
    structure Ptr = MLton.Pointer

    type addr = Ptr.t
    val null = Ptr.null : addr
    fun isNull a = a = null
    infix ++ --
    (* rely on 2's-complement for the following... *)
    fun (a: addr) ++ i = Ptr.add (a, Word32.fromInt i)
    val compare = Ptr.compare
    fun a1 -- a2 = Word32.toIntX (Ptr.diff (a1, a2))

    val addr_size = 0w4
    val char_size = 0w1
    val short_size = 0w2
    val int_size = 0w4
    val long_size = 0w4
    val longlong_size = 0w8
    val float_size = 0w4
    val double_size = 0w8

    local 
       fun get g addr =
          g (addr, 0)
    in
       val load_addr = get Ptr.getPointer
       val load_uchar = get Ptr.getWord8
       val load_schar = get Ptr.getInt8
       val load_ushort = get Ptr.getWord16
       val load_sshort = get Ptr.getInt16
       val load_uint = get Ptr.getWord32
       val load_sint = get Ptr.getInt32
       val load_ulong = get Ptr.getWord32
       val load_slong = get Ptr.getInt32
       val load_ulonglong = get Ptr.getWord64
       val load_slonglong = get Ptr.getInt64
       val load_float = get Ptr.getReal32
       val load_double = get Ptr.getReal64
    end

    local
       fun set s (addr, x) =
          s (addr, 0, x)
    in
       val store_addr = set Ptr.setPointer
       val store_uchar = set Ptr.setWord8
       val store_schar = set Ptr.setInt8
       val store_ushort = set Ptr.setWord16
       val store_sshort = set Ptr.setInt16
       val store_uint = set Ptr.setWord32
       val store_sint = set Ptr.setInt32
       val store_ulong = set Ptr.setWord32
       val store_slong = set Ptr.setInt32
       val store_ulonglong = set Ptr.setWord64
       val store_slonglong = set Ptr.setInt64
       val store_float = set Ptr.setReal32
       val store_double = set Ptr.setReal64
    end

    val int_bits = Word.fromInt Word32.wordSize

    (* this needs to be severely optimized... *)
    fun bcopy { from: addr, to: addr, bytes: word } =
        if bytes > 0w0 then
            (store_uchar (to, load_uchar from);
             bcopy { from = from ++ 1, to = to ++ 1, bytes = bytes - 0w1 })
        else ()

    (* types used in C calling convention *)
    type cc_addr = MLton.Pointer.t
    type cc_schar = Int8.int
    type cc_uchar = Word8.word
    type cc_sshort = Int16.int
    type cc_ushort = Word16.word
    type cc_sint = Int32.int
    type cc_uint = Word32.word
    type cc_slong = Int32.int
    type cc_ulong = Word32.word
    type cc_slonglong = Int64.int
    type cc_ulonglong = Word64.word
    type cc_float = Real32.real
    type cc_double = Real64.real

    (* wrapping and unwrapping for cc types *)
    fun wrap_addr (x : addr) = x : cc_addr
    fun wrap_schar (x : MLRep.Char.Signed.int) = x : cc_schar
    fun wrap_uchar (x : MLRep.Char.Unsigned.word) = x : cc_uchar
    fun wrap_sshort (x : MLRep.Short.Signed.int) = x : cc_sshort
    fun wrap_ushort (x : MLRep.Short.Unsigned.word) = x : cc_ushort
    fun wrap_sint (x : MLRep.Int.Signed.int) = x : cc_sint
    fun wrap_uint (x : MLRep.Int.Unsigned.word) = x : cc_uint
    fun wrap_slong (x : MLRep.Long.Signed.int) = x : cc_slong
    fun wrap_ulong (x : MLRep.Long.Unsigned.word) = x : cc_ulong
    fun wrap_slonglong (x : MLRep.LongLong.Signed.int) = x : cc_slonglong
    fun wrap_ulonglong (x : MLRep.LongLong.Unsigned.word) = x : cc_ulonglong
    fun wrap_float (x : MLRep.Float.real) = x : cc_float
    fun wrap_double (x : MLRep.Double.real) = x : cc_double

    fun unwrap_addr (x : cc_addr) = x : addr
    fun unwrap_schar (x : cc_schar) = x : MLRep.Char.Signed.int
    fun unwrap_uchar (x : cc_uchar) = x : MLRep.Char.Unsigned.word
    fun unwrap_sshort (x : cc_sshort) = x : MLRep.Short.Signed.int
    fun unwrap_ushort (x : cc_ushort) = x : MLRep.Short.Unsigned.word
    fun unwrap_sint (x : cc_sint) = x : MLRep.Int.Signed.int
    fun unwrap_uint (x : cc_uint) = x : MLRep.Int.Unsigned.word
    fun unwrap_slong (x : cc_slong) = x : MLRep.Long.Signed.int
    fun unwrap_ulong (x : cc_ulong) = x : MLRep.Long.Unsigned.word
    fun unwrap_slonglong (x : cc_slonglong) = x : MLRep.LongLong.Signed.int
    fun unwrap_ulonglong (x : cc_ulonglong) = x : MLRep.LongLong.Unsigned.word
    fun unwrap_float (x : cc_float) = x : MLRep.Float.real
    fun unwrap_double (x : cc_double) = x : MLRep.Double.real

    fun p2i (x : addr) : MLRep.Int.Unsigned.word = Ptr.diff (x, null) 
    fun i2p (x : MLRep.Int.Unsigned.word) : addr = Ptr.add (null, x)
end
