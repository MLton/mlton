(* memaccess.sml
 * 2007 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.  Make use of $(SML_LIB)/basis/c-types.mlb
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
    fun (a: addr) ++ i = Ptr.add (a, Word.fromInt i)
    val compare = Ptr.compare
    fun a1 -- a2 = Word.toIntX (Ptr.diff (a1, a2))

    val addr_size = Word.fromInt (C_Size.wordSize div 8)
    val char_size = Word.fromInt (C_UChar.wordSize div 8)
    val short_size = Word.fromInt (C_UShort.wordSize div 8)
    val int_size = Word.fromInt (C_UInt.wordSize div 8)
    val long_size = Word.fromInt (C_ULong.wordSize div 8)
    val longlong_size = Word.fromInt (C_ULongLong.wordSize div 8)
    local
       structure RealNArg =
          struct
             type 'a t = int
             val fReal32 = 32
             val fReal64 = 64
          end
       structure Float = C_Float_ChooseRealN(RealNArg)
       structure Double = C_Double_ChooseRealN(RealNArg)
    in
       val float_size = Word.fromInt (Float.f div 8)
       val double_size = Word.fromInt (Double.f div 8)
    end

    local 
       fun get g addr =
          g (addr, 0)
       structure IntNArg = 
          struct
             type 'a t = Ptr.t * int -> 'a
             val fInt8 = Ptr.getInt8
             val fInt16 = Ptr.getInt16
             val fInt32 = Ptr.getInt32
             val fInt64 = Ptr.getInt64
          end
       structure RealNArg = 
          struct
             type 'a t = Ptr.t * int -> 'a
             val fReal32 = Ptr.getReal32
             val fReal64 = Ptr.getReal64
          end
       structure WordNArg = 
          struct
             type 'a t = Ptr.t * int -> 'a
             val fWord8 = Ptr.getWord8
             val fWord16 = Ptr.getWord16
             val fWord32 = Ptr.getWord32
             val fWord64 = Ptr.getWord64
          end
       structure UChar = C_UChar_ChooseWordN(WordNArg)
       structure SChar = C_SChar_ChooseIntN(IntNArg)
       structure UShort = C_UShort_ChooseWordN(WordNArg)
       structure SShort = C_SShort_ChooseIntN(IntNArg)
       structure UInt = C_UInt_ChooseWordN(WordNArg)
       structure SInt = C_SInt_ChooseIntN(IntNArg)
       structure ULong = C_ULong_ChooseWordN(WordNArg)
       structure SLong = C_SLong_ChooseIntN(IntNArg)
       structure ULongLong = C_ULongLong_ChooseWordN(WordNArg)
       structure SLongLong = C_SLongLong_ChooseIntN(IntNArg)
       structure Float = C_Float_ChooseRealN(RealNArg)
       structure Double = C_Double_ChooseRealN(RealNArg)
    in
       val load_addr = get Ptr.getPointer
       val load_uchar = get UChar.f
       val load_schar = get SChar.f
       val load_ushort = get UShort.f
       val load_sshort = get SShort.f
       val load_uint = get UInt.f
       val load_sint = get SInt.f
       val load_ulong = get ULong.f
       val load_slong = get SLong.f
       val load_ulonglong = get ULongLong.f
       val load_slonglong = get SLongLong.f
       val load_float = get Float.f
       val load_double = get Double.f
    end

    local
       fun set s (addr, x) =
          s (addr, 0, x)
       structure IntNArg = 
          struct
             type 'a t = Ptr.t * int * 'a -> unit
             val fInt8 = Ptr.setInt8
             val fInt16 = Ptr.setInt16
             val fInt32 = Ptr.setInt32
             val fInt64 = Ptr.setInt64
          end
       structure RealNArg = 
          struct
             type 'a t = Ptr.t * int * 'a -> unit
             val fReal32 = Ptr.setReal32
             val fReal64 = Ptr.setReal64
          end
       structure WordNArg = 
          struct
             type 'a t = Ptr.t * int * 'a -> unit
             val fWord8 = Ptr.setWord8
             val fWord16 = Ptr.setWord16
             val fWord32 = Ptr.setWord32
             val fWord64 = Ptr.setWord64
          end
       structure UChar = C_UChar_ChooseWordN(WordNArg)
       structure SChar = C_SChar_ChooseIntN(IntNArg)
       structure UShort = C_UShort_ChooseWordN(WordNArg)
       structure SShort = C_SShort_ChooseIntN(IntNArg)
       structure UInt = C_UInt_ChooseWordN(WordNArg)
       structure SInt = C_SInt_ChooseIntN(IntNArg)
       structure ULong = C_ULong_ChooseWordN(WordNArg)
       structure SLong = C_SLong_ChooseIntN(IntNArg)
       structure ULongLong = C_ULongLong_ChooseWordN(WordNArg)
       structure SLongLong = C_SLongLong_ChooseIntN(IntNArg)
       structure Float = C_Float_ChooseRealN(RealNArg)
       structure Double = C_Double_ChooseRealN(RealNArg)
    in
       val store_addr = set Ptr.setPointer
       val store_uchar = set UChar.f
       val store_schar = set SChar.f
       val store_ushort = set UShort.f
       val store_sshort = set SShort.f
       val store_uint = set UInt.f
       val store_sint = set SInt.f
       val store_ulong = set ULong.f
       val store_slong = set SLong.f
       val store_ulonglong = set ULongLong.f
       val store_slonglong = set SLongLong.f
       val store_float = set Float.f
       val store_double = set Double.f
    end

    val int_bits = int_size * 0w8

    (* this needs to be severely optimized... *)
    fun bcopy { from: addr, to: addr, bytes: word } =
        if bytes > 0w0 then
            (store_uchar (to, load_uchar from);
             bcopy { from = from ++ 1, to = to ++ 1, bytes = bytes - 0w1 })
        else ()

    (* types used in C calling convention *)
    type cc_addr = MLton.Pointer.t
    type cc_schar = C_SChar.int
    type cc_uchar = C_UChar.word
    type cc_sshort = C_SShort.int
    type cc_ushort = C_UShort.word
    type cc_sint = C_SInt.int
    type cc_uint = C_UInt.word
    type cc_slong = C_SLong.int
    type cc_ulong = C_ULong.word
    type cc_slonglong = C_SLongLong.int
    type cc_ulonglong = C_ULongLong.word
    type cc_float = C_Float.real
    type cc_double = C_Double.real

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

    fun p2i (x : addr) : MLRep.Long.Unsigned.word = 
       C_ULong.fromLarge (Word.toLarge (Ptr.diff (x, null)))
    fun i2p (x : MLRep.Long.Unsigned.word) : addr = 
       Ptr.add (null, Word.fromLarge (C_ULong.toLarge x))
end
