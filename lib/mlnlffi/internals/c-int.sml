(* c-int.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(*
 * The implementation of the interface that encodes C's type system
 * in ML.  This implementation includes its "private" extensions.
 *
 *   (C) 2001, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
local
(* We play some games here with first calling C_Int simply C and then
 * renaming it because they result in saner printing behavior. *)
structure C :> C_INT = struct

    exception OutOfMemory = CMemory.OutOfMemory

    fun bug m = raise Fail ("impossible: " ^ m)

    type addr = CMemory.addr

    local
	datatype 'f objt =
	    BASE of word
	  | PTR of 'f objt
	  | FPTR of addr -> 'f
	  | ARR of { typ: 'f objt, n: word, esz: word, asz: word }

	(* Bitfield: b bits wide, l bits from left corner, r bits from right.
	 * The word itself is CMemory.int_bits wide and located at address a.
	 *
	 *    MSB                         LSB
	 *     V        |<---b--->|        V
	 *    |<---l---> ......... <---r--->|
	 *    |<----------wordsize--------->|
	 * 
	 *     0.......0 1.......1 0.......0    = m
	 *     1.......1 0.......0 1.......1    = im
	 *
	 * l + r = lr *)
	type cword = MLRep.Int.Unsigned.word
	type bf = { a: addr, l: word, r: word, lr: word, m: cword, im: cword }

	fun pair_type_addr (t: 'f objt) (a: addr) = (a, t)
	fun strip_type (a: addr, _: 'f objt) = a
	fun p_strip_type (a: addr, _: 'f objt) = a
	fun strip_fun (a: addr, _: 'f) = a
	fun addr_type_id (x: addr * 'f objt) = x
	fun addr_id (x: addr) = x

	infix -- ++
	val op -- = CMemory.--
	val op ++ = CMemory.++

	infix << >> ~>> && || ^^
	val op << = MLRep.Int.Unsigned.<<
	val op >> = MLRep.Int.Unsigned.>>
	val op ~>> = MLRep.Int.Unsigned.~>>
	val op && = MLRep.Int.Unsigned.andb
	val op || = MLRep.Int.Unsigned.orb
	val op ^^ = MLRep.Int.Unsigned.xorb
	val ~~ = MLRep.Int.Unsigned.notb
    in

    type ('t, 'f, 'c) obj  = addr * 'f objt	(* RTTI for stored value *)
    type ('t, 'f, 'c) obj' = addr

    type naf = unit
    type ro = unit
    type rw = unit

    type ('o, 'f) ptr  = addr * 'f objt		(* RTTI for target value *)
    type ('o, 'f) ptr' = addr

    type ('t, 'n) arr = unit

    type 'f fptr = addr * 'f
    type 'f fptr' = addr		(* does not carry function around *)

    type void = unit
    type voidptr = (void, unit) ptr'

    type 'tag su = unit

    type 'tag enum = MLRep.Int.Signed.int

    type schar     = MLRep.Char.Signed.int
    type uchar     = MLRep.Char.Unsigned.word
    type sshort    = MLRep.Short.Signed.int
    type ushort    = MLRep.Short.Unsigned.word
    type sint      = MLRep.Int.Signed.int
    type uint      = MLRep.Int.Unsigned.word
    type slong     = MLRep.Long.Signed.int
    type ulong     = MLRep.Long.Unsigned.word
    type slonglong = MLRep.LongLong.Signed.int
    type ulonglong = MLRep.LongLong.Unsigned.word
    type float     = MLRep.Float.real
    type double    = MLRep.Double.real

    type 'c schar_obj      = (    schar, naf, 'c) obj
    type 'c uchar_obj      = (    uchar, naf, 'c) obj
    type 'c sshort_obj     = (   sshort, naf, 'c) obj
    type 'c ushort_obj     = (   ushort, naf, 'c) obj
    type 'c sint_obj       = (     sint, naf, 'c) obj
    type 'c uint_obj       = (     uint, naf, 'c) obj
    type 'c slong_obj      = (    slong, naf, 'c) obj
    type 'c ulong_obj      = (    ulong, naf, 'c) obj
    type 'c slonglong_obj  = (slonglong, naf, 'c) obj
    type 'c ulonglong_obj  = (ulonglong, naf, 'c) obj
    type 'c float_obj      = (    float, naf, 'c) obj
    type 'c double_obj     = (   double, naf, 'c) obj
    type 'c voidptr_obj    = (  voidptr, naf, 'c) obj
    type ('e, 'c) enum_obj = (  'e enum, naf, 'c) obj
    type ('f, 'c) fptr_obj = (  'f fptr,  'f, 'c) obj
    type ('s, 'c) su_obj   = (    's su, naf, 'c) obj

    type 'c schar_obj'      = (    schar, naf, 'c) obj'
    type 'c uchar_obj'      = (    uchar, naf, 'c) obj'
    type 'c sshort_obj'     = (   sshort, naf, 'c) obj'
    type 'c ushort_obj'     = (   ushort, naf, 'c) obj'
    type 'c sint_obj'       = (     sint, naf, 'c) obj'
    type 'c uint_obj'       = (     uint, naf, 'c) obj'
    type 'c slong_obj'      = (    slong, naf, 'c) obj'
    type 'c ulong_obj'      = (    ulong, naf, 'c) obj'
    type 'c slonglong_obj'  = (slonglong, naf, 'c) obj'
    type 'c ulonglong_obj'  = (ulonglong, naf, 'c) obj'
    type 'c float_obj'      = (    float, naf, 'c) obj'
    type 'c double_obj'     = (   double, naf, 'c) obj'
    type 'c voidptr_obj'    = (  voidptr, naf, 'c) obj'
    type ('e, 'c) enum_obj' = (  'e enum, naf, 'c) obj'
    type ('f, 'c) fptr_obj' = (  'f fptr,  'f, 'c) obj'
    type ('s, 'c) su_obj'   = (    's su, naf, 'c) obj'

    type 'c ubf = bf
    type 'c sbf = bf

    structure W = struct
        type ('from, 'to) witness = unit

	val trivial = ()
	fun pointer () = ()
	fun object () = ()
	fun arr () = ()
	fun ro () = ()
	fun rw () = ()
    end

    fun convert w x = x
    fun convert' w x = x

    (*
     * A family of types and corresponding values representing natural numbers.
     *   (An encoding in SML without using dependent types.)
     *  This is the full implementation including an unsafe extension
     * ("fromInt"). *)

    structure Dim = struct

        type ('a, 'z) dim0 = int
	fun toInt d = d
	fun fromInt d = d

	type dec = unit
	type 'a dg0 = unit
	type 'a dg1 = unit
	type 'a dg2 = unit
	type 'a dg3 = unit
	type 'a dg4 = unit
	type 'a dg5 = unit
	type 'a dg6 = unit
	type 'a dg7 = unit
	type 'a dg8 = unit
	type 'a dg9 = unit

	type zero = unit
	type nonzero = unit

	type 'a dim = ('a, nonzero) dim0

	local
	    fun dg n d = 10 * d + n
	in
            val dec' = 0
            val (dg0', dg1', dg2', dg3', dg4', dg5', dg6', dg7', dg8', dg9') =
		(dg 0, dg 1, dg 2, dg 3, dg 4, dg 5, dg 6, dg 7, dg 8, dg 9)

	    fun dec k = k dec'
	    fun dg0 d k = k (dg0' d)
	    fun dg1 d k = k (dg1' d)
	    fun dg2 d k = k (dg2' d)
	    fun dg3 d k = k (dg3' d)
	    fun dg4 d k = k (dg4' d)
	    fun dg5 d k = k (dg5' d)
	    fun dg6 d k = k (dg6' d)
	    fun dg7 d k = k (dg7' d)
	    fun dg8 d k = k (dg8' d)
	    fun dg9 d k = k (dg9' d)
	    fun dim d = d
	end
    end

    structure S = struct

        type 't size = word

	fun toWord (s: 't size) = s

	val schar     = CMemory.char_size
	val uchar     = CMemory.char_size
	val sint      = CMemory.int_size
	val uint      = CMemory.int_size
	val sshort    = CMemory.short_size
	val ushort    = CMemory.short_size
	val slong     = CMemory.long_size
	val ulong     = CMemory.long_size
	val slonglong = CMemory.longlong_size
	val ulonglong = CMemory.longlong_size
	val float     = CMemory.float_size
	val double    = CMemory.double_size

	val voidptr = CMemory.addr_size
	val ptr = CMemory.addr_size
	val fptr = CMemory.addr_size
	val enum = CMemory.int_size
    end

    structure T = struct

        type ('t, 'f) typ = 'f objt

	fun typeof (_: addr, t: 'f objt) = t

	fun sizeof (BASE b) = b
	  | sizeof (PTR _) = S.ptr
	  | sizeof (FPTR _) = S.fptr
	  | sizeof (ARR a) = #asz a

	(* use private (and unsafe) extension to Dim module here... *)
	fun dim (ARR { n, ... }) = Dim.fromInt (Word.toInt n)
	  | dim _ = bug "T.dim (non-array type)"

	fun pointer t = PTR t
	fun target (PTR t) = t
	  | target _ = bug "T.target (non-pointer type)"
	fun arr (t, d) = let
	    val n = Word.fromInt (Dim.toInt d)
	    val s = sizeof t
	in
	    ARR { typ = t, n = n, esz = s, asz = n * s }
	end
	fun elem (ARR a) = #typ a
	  | elem _ = bug "T.elem (non-array type)"
	fun ro (t: 'f objt) = t

	val schar     = BASE S.schar
	val uchar     = BASE S.uchar
	val sshort    = BASE S.sshort
	val ushort    = BASE S.ushort
	val sint      = BASE S.sint
	val uint      = BASE S.uint
	val slong     = BASE S.slong
	val ulong     = BASE S.ulong
	val slonglong = BASE S.slonglong
	val ulonglong = BASE S.ulonglong
	val float     = BASE S.float
	val double    = BASE S.double

	val voidptr = BASE S.voidptr

	val enum = BASE S.sint
    end

    structure Light = struct
        val obj = p_strip_type
	val ptr = p_strip_type
	val fptr = strip_fun
    end

    structure Heavy = struct
        val obj = pair_type_addr
	fun ptr (PTR t) p = (p, t)
	  | ptr _ _ = bug "Heavy.ptr (non-object-pointer-type)"
	fun fptr (FPTR mkf) p = (p, mkf p)
	  | fptr _ _ = bug "Heavy.fptr (non-function-pointer-type)"
    end

    fun sizeof (_: addr, t) = T.sizeof t

    structure Cvt = struct
        (* going between abstract and concrete; these are all identities *)
        fun c_schar (c: schar) = c
	fun c_uchar (c: uchar) = c
        fun c_sshort (s: sshort) = s
	fun c_ushort (s: ushort) = s
        fun c_sint (i: sint) = i
	fun c_uint (i: uint) = i
        fun c_slong (l: slong) = l
	fun c_ulong (l: ulong) = l
        fun c_slonglong (ll: slonglong) = ll
	fun c_ulonglong (ll: ulonglong) = ll
	fun c_float (f: float) = f
	fun c_double (d: double) = d
	fun i2c_enum (e: 'e enum) = e

	val ml_schar = c_schar
	val ml_uchar = c_uchar
	val ml_sshort = c_sshort
	val ml_ushort = c_ushort
	val ml_sint = c_sint
	val ml_uint = c_uint
	val ml_slong = c_slong
	val ml_ulong = c_ulong
	val ml_slonglong = c_slonglong
	val ml_ulonglong = c_ulonglong
	val ml_float = c_float
	val ml_double = c_double
	val c2i_enum = i2c_enum
    end

    structure Get = struct
        val uchar' = CMemory.load_uchar
	val schar' = CMemory.load_schar
	val uint' = CMemory.load_uint
	val sint' = CMemory.load_sint
	val ushort' = CMemory.load_ushort
	val sshort' = CMemory.load_sshort
	val ulong' = CMemory.load_ulong
	val slong' = CMemory.load_slong
	val ulonglong' = CMemory.load_ulonglong
	val slonglong' = CMemory.load_slonglong
	val float' = CMemory.load_float
	val double' = CMemory.load_double
	val enum' = CMemory.load_sint

	val ptr' = CMemory.load_addr
	val fptr' = CMemory.load_addr
	val voidptr' = CMemory.load_addr

	val uchar = uchar' o strip_type
	val schar = schar' o strip_type
	val uint = uint' o strip_type
	val sint = sint' o strip_type
	val ushort = ushort' o strip_type
	val sshort = sshort' o strip_type
	val ulong = ulong' o strip_type
	val slong = slong' o strip_type
	val ulonglong = ulonglong' o strip_type
	val slonglong = slonglong' o strip_type
	val float = float' o strip_type
	val double = double' o strip_type
	val voidptr = voidptr' o strip_type
	val enum = enum' o strip_type

	fun ptr (a, PTR t) = (ptr' a, t)
	  | ptr _ = bug "Get.ptr (non-pointer)"
	fun fptr (a, FPTR mkf) =
	    let val fa = fptr' a in (fa, mkf fa) end
	  | fptr _ = bug "Get.fptr (non-function-pointer)"

	local
	    val u2s = MLRep.Int.Signed.fromLarge o MLRep.Int.Unsigned.toLargeIntX
	in
	    fun ubf ({ a, l, r, lr, m, im } : bf) =
		(CMemory.load_uint a << l) >> lr
	    fun sbf ({ a, l, r, lr, m, im } : bf) =
		u2s ((CMemory.load_uint a << l) ~>> lr)
	end
    end

    structure Set = struct
        val uchar' = CMemory.store_uchar
	val schar' = CMemory.store_schar
	val uint' = CMemory.store_uint
	val sint' = CMemory.store_sint
	val ushort' = CMemory.store_ushort
	val sshort' = CMemory.store_sshort
	val ulong' = CMemory.store_ulong
	val slong' = CMemory.store_slong
	val ulonglong' = CMemory.store_ulonglong
	val slonglong' = CMemory.store_slonglong
	val float' = CMemory.store_float
	val double' = CMemory.store_double
	val enum' = CMemory.store_sint

	val ptr' = CMemory.store_addr
	val fptr' = CMemory.store_addr
	val voidptr' = CMemory.store_addr
	val ptr_voidptr' = CMemory.store_addr

	local
	    infix $
	    fun (f $ g) (x, y) = f (g x, y)
	in
	    val uchar = uchar' $ strip_type
	    val schar = schar' $ strip_type
	    val uint = uint' $ strip_type
	    val sint = sint' $ strip_type
	    val ushort = ushort' $ strip_type
	    val sshort = sshort' $ strip_type
	    val ulong = ulong' $ strip_type
	    val slong = slong' $ strip_type
	    val ulonglong = ulonglong' $ strip_type
	    val slonglong = slonglong' $ strip_type
	    val float = float' $ strip_type
	    val double = double' $ strip_type
	    val voidptr = voidptr' $ strip_type
	    val enum = enum' $ strip_type

	    fun ptr_voidptr (x, p) = ptr_voidptr' (p_strip_type x, p)

	    fun ptr (x, p) = ptr' (p_strip_type x, p_strip_type p)
	    fun fptr (x, f) = fptr' (p_strip_type x, strip_fun f)
	end

	fun ubf ({ a, l, r, lr, m, im }, x) =
	    CMemory.store_uint (a, (CMemory.load_uint a && im) ||
				   ((x << r) && m))

	local
	    val s2u = MLRep.Int.Unsigned.fromLargeInt o MLRep.Int.Signed.toLarge
	in
	    fun sbf (f, x) = ubf (f, s2u x)
	end
    end

    fun copy' bytes { from, to } =
	CMemory.bcopy { from = from, to = to, bytes = bytes }
    fun copy { from = (from, t), to = (to, _: 'f objt) } =
	copy' (T.sizeof t) { from = from, to = to }

    val ro = addr_type_id
    val rw = addr_type_id

    val ro' = addr_id
    val rw' = addr_id

    structure Ptr = struct
        val |&| = addr_type_id
	val |*| = addr_type_id

	val |&! = addr_id
	val |*! = addr_id

	fun compare (p, p') = CMemory.compare (p_strip_type p, p_strip_type p')

	val compare' = CMemory.compare

	val inject' = addr_id
	val cast' = addr_id

	val inject = p_strip_type
	fun cast (PTR t) (p : voidptr) = (p, t)
	  | cast _ _ = bug "Ptr.cast (non-pointer-type)"

	val vnull = CMemory.null
	fun null t = cast t vnull
	val null' = vnull

	val fnull' = CMemory.null
	fun fnull t = Heavy.fptr t fnull'

	val isVNull = CMemory.isNull
	fun isNull p = isVNull (inject p)
	val isNull' = CMemory.isNull

	fun isFNull (p, _) = CMemory.isNull p
	val isFNull' = CMemory.isNull

	fun |+! s (p, i) = p ++ (Word.toInt s * i)
	fun |-! s (p, p') = (p -- p') div Word.toInt s

	fun |+| ((p, t), i) = (|+! (T.sizeof t) (p, i), t)
	fun |-| ((p, t), (p', _: 'f objt)) = |-! (T.sizeof t) (p, p')

	fun sub (p, i) = |*| (|+| (p, i))

	fun sub' t (p, i) = |*! (|+! t (p, i))

	val ro = addr_type_id
	val rw = addr_type_id

	val ro' = addr_id
	val rw' = addr_id

	fun convert w x = x
	fun convert' w x = x
    end

    structure Arr = struct
        local
	    fun asub (a, i, n, esz) =
		(* take advantage of wrap-around to avoid the >= 0 test... *)
		if Word.fromInt i < n then a ++ (Word.toIntX esz * i)
		else raise General.Subscript
	in
	    fun sub ((a, ARR { typ, n, esz, ... }), i) = (asub (a, i, n, esz), typ)
	      | sub _ = bug "Arr.sub (non-array)"
	    fun sub' (s, d) (a, i) = asub (a, i, Word.fromInt (Dim.toInt d), s)
	end

	fun decay (a, ARR { typ, ... }) = (a, typ)
	  | decay _ = bug "Arr.decay (non-array)"

        val decay' = addr_id

	fun reconstruct ((a: addr, t), d) = (a, T.arr (t, d))

	fun reconstruct' (a: addr, d: 'n Dim.dim) = a

	fun dim (_: addr, t) = T.dim t
    end

    fun new' s = CMemory.alloc s
    fun new t = (new' (T.sizeof t), t)

    val discard' = CMemory.free
    fun discard x = discard' (p_strip_type x)

    fun alloc' s i = CMemory.alloc (s * i)
    fun alloc t i = (alloc' (T.sizeof t) i, t)

    val free' = CMemory.free
    fun free x = free' (p_strip_type x)

    fun call ((_: addr, f), x) = f x

    fun call' (FPTR mkf) (a, x) = mkf a x
      | call' _ _ = bug "call' (non-function-pointer-type)"

    structure U = struct
        fun fcast (f : 'fa fptr') : 'fb fptr' = f
	fun p2i (a : ('o, 'f) ptr') : ulong = CMemory.p2i a
	fun i2p (a : ulong) : ('o, 'f) ptr' = CMemory.i2p a
    end

    (* ------------- internal stuff ------------- *)

    fun mk_obj' (a : addr) = a
    fun mk_voidptr (a : addr) = a
    fun mk_fptr (mkf, a) = (a, mkf a)

    local
	fun mk_field (t: 'f objt, i, (a, _: naf objt)) = (a ++ i, t)
    in
        val mk_rw_field = mk_field
	val mk_ro_field = mk_field
	fun mk_field' (i, a) = a ++ i
    end

    local
	fun mk_bf' (offset, bits, shift) a = let
	    val a = a ++ offset
	    val l = shift
	    val lr = CMemory.int_bits - bits
	    val r = lr - l
	    val m = (~~0w0 << lr) >> l
	    val im = ~~ m
	in
	    { a = a, l = l, r = r, lr = lr, m = m, im = im } : bf
	end
	fun mk_bf acc (a, _: naf objt) = mk_bf' acc a
    in
        val mk_rw_ubf = mk_bf
	val mk_ro_ubf = mk_bf
	val mk_rw_ubf' = mk_bf'
	val mk_ro_ubf' = mk_bf'

        val mk_rw_sbf = mk_bf
	val mk_ro_sbf = mk_bf
	val mk_rw_sbf' = mk_bf'
	val mk_ro_sbf' = mk_bf'
    end

    fun mk_su_size sz = sz
    fun mk_su_typ sz = BASE sz
    fun mk_fptr_typ (mkf: addr -> 'a -> 'b) = FPTR mkf

    val reveal = addr_id
    val freveal = addr_id

    val vcast = addr_id
    val pcast = addr_id
    val fcast = addr_id

    fun unsafe_sub esz (a, i) = a ++ esz * i
    end (* local *)
end
in
structure C_Int = C
end
