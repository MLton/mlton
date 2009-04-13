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
    val null = CMemory.null

    local
        datatype 'f objt =
            BASE of word
          | PTR of 'f
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

(*
        fun pair_type_addr (t: 'f objt) (a: addr) = (a, t)
*)
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
(*
        val op ^^ = MLRep.Int.Unsigned.xorb
*)
        val ~~ = MLRep.Int.Unsigned.notb
    in

    type ('t, 'c) obj  = addr * 't objt (* RTTI for stored value *)
    type ('t, 'c) obj' = addr
    type ro = unit
    type rw = unit

    type 'o ptr  = addr * 'o objt (* RTTI for target value *)
    type 'o ptr' = addr

    type ('t, 'n) arr = 't

    type 'f fptr = addr * 'f
    type 'f fptr' = addr

    type void = unit
    type voidptr = void ptr'

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

    type 'c schar_obj      = (    schar, 'c) obj
    type 'c uchar_obj      = (    uchar, 'c) obj
    type 'c sshort_obj     = (   sshort, 'c) obj
    type 'c ushort_obj     = (   ushort, 'c) obj
    type 'c sint_obj       = (     sint, 'c) obj
    type 'c uint_obj       = (     uint, 'c) obj
    type 'c slong_obj      = (    slong, 'c) obj
    type 'c ulong_obj      = (    ulong, 'c) obj
    type 'c slonglong_obj  = (slonglong, 'c) obj
    type 'c ulonglong_obj  = (ulonglong, 'c) obj
    type 'c float_obj      = (    float, 'c) obj
    type 'c double_obj     = (   double, 'c) obj
    type 'c voidptr_obj    = (  voidptr, 'c) obj
    type ('e, 'c) enum_obj = (  'e enum, 'c) obj
    type ('f, 'c) fptr_obj = (  'f fptr, 'c) obj
    type ('s, 'c) su_obj   = (    's su, 'c) obj

    type 'c schar_obj'      = (    schar, 'c) obj'
    type 'c uchar_obj'      = (    uchar, 'c) obj'
    type 'c sshort_obj'     = (   sshort, 'c) obj'
    type 'c ushort_obj'     = (   ushort, 'c) obj'
    type 'c sint_obj'       = (     sint, 'c) obj'
    type 'c uint_obj'       = (     uint, 'c) obj'
    type 'c slong_obj'      = (    slong, 'c) obj'
    type 'c ulong_obj'      = (    ulong, 'c) obj'
    type 'c slonglong_obj'  = (slonglong, 'c) obj'
    type 'c ulonglong_obj'  = (ulonglong, 'c) obj'
    type 'c float_obj'      = (    float, 'c) obj'
    type 'c double_obj'     = (   double, 'c) obj'
    type 'c voidptr_obj'    = (  voidptr, 'c) obj'
    type ('e, 'c) enum_obj' = (  'e enum, 'c) obj'
    type ('f, 'c) fptr_obj' = (  'f fptr, 'c) obj'
    type ('s, 'c) su_obj'   = (    's su, 'c) obj'

    type 'c ubf = bf
    type 'c sbf = bf

    structure W = struct
        type ('from, 'to) witness = 'from -> 'to

        fun convert (w : 's -> 't) (x : 's objt) : 't objt =
           case x of
              BASE b => BASE b
            | PTR x => PTR (w x)
            | FPTR f => FPTR (fn a => w (f a))
            | ARR {typ, n, esz, asz} =>
                 ARR {typ = convert w typ, 
                      n = n, esz = esz, asz = asz}

        val trivial : ('t, 't) witness = 
           fn x => x

        val pointer : ('from, 'to) witness -> ('from ptr, 'to ptr) witness =
           fn w => fn (a, t) => (a, convert w t)
        val object : ('from, 'to) witness -> (('from, 'c) obj, ('to, 'c) obj) witness =
           fn w => fn (a, t) => (a, convert w t)
        val arr : ('from, 'to) witness -> (('from, 'n) arr, ('to, 'n) arr) witness =
           fn w => w
        val ro : ('from, 'to) witness -> (('from, 'fc) obj, ('to, ro) obj) witness =
           fn w => fn (a, t) => (a, convert w t)
        val rw : ('from, 'to) witness -> (('from, 'fc) obj, ('to, 'tc) obj) witness =
           fn w => fn (a, t) => (a, convert w t)
    end

    val convert : (('st, 'sc) obj, ('tt, 'tc) obj) W.witness -> 
                  ('st, 'sc) obj -> ('tt, 'tc) obj =
       fn w => fn x => w x
    val convert' : (('st, 'sc) obj, ('tt, 'tc) obj) W.witness -> 
                   ('st, 'sc) obj' -> ('tt, 'tc) obj' =
       fn _ => fn x => x

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

        type 't typ = 't objt

        val typeof : ('t, 'c) obj -> 't typ =
           fn (_, t) => t

        val sizeof : 't typ -> 't S.size =
           fn BASE b => b
            | PTR _ => S.ptr
            | FPTR _ => S.fptr
            | ARR a => #asz a

        (* use private (and unsafe) extension to Dim module here... *)
        val dim : ('t, 'n) arr typ -> 'n Dim.dim =
           fn ARR { n, ... } => Dim.fromInt (Word.toInt n)
            | _ => bug "T.dim (non-array type)"

        val pointer : 't typ -> ('t, rw) obj ptr typ =
           fn t => PTR (null, PTR (null, t))
        val target : ('t, 'c) obj ptr typ -> 't typ =
           fn PTR (_, PTR (_, t)) => t
            | _ => bug "T.target (non-pointer type)"
        val arr : 't typ * 'n Dim.dim -> ('t, 'n) arr typ =
           fn (t, d) =>
           let
              val n = Word.fromInt (Dim.toInt d)
              val s = sizeof t
           in
              ARR { typ = t, n = n, esz = s, asz = n * s }
           end
        val elem : ('t, 'n) arr typ -> 't typ =
           fn ARR a => #typ a
            | _ => bug "T.elem (non-array type)"
        val ro : ('t, 'c) obj ptr typ -> ('t, ro) obj ptr typ =
           fn t => t

        val schar     : schar typ     = BASE S.schar
        val uchar     : uchar typ     = BASE S.uchar
        val sshort    : sshort typ    = BASE S.sshort
        val ushort    : ushort typ    = BASE S.ushort
        val sint      : sint typ      = BASE S.sint
        val uint      : uint typ      = BASE S.uint
        val slong     : slong typ     = BASE S.slong
        val ulong     : ulong typ     = BASE S.ulong
        val slonglong : slonglong typ = BASE S.slonglong
        val ulonglong : ulonglong typ = BASE S.ulonglong
        val float     : float typ     = BASE S.float
        val double    : double typ    = BASE S.double

        val voidptr : voidptr typ = BASE S.voidptr

        val enum : 'tag enum typ = BASE S.sint
    end

    structure Light = struct
        val obj : ('t, 'c) obj -> ('t, 'c) obj' =
           fn (a, _) => a
        val ptr : 'o ptr -> 'o ptr' =
           fn (a, _) => a
        val fptr : 'f fptr -> 'f fptr' =
           fn (a, _) => a
    end

    structure Heavy = struct
        val obj : 't T.typ -> ('t, 'c) obj' -> ('t, 'c) obj = 
           fn t => fn a => (a, t)
        val ptr : 'o ptr T.typ -> 'o ptr' -> 'o ptr =
           fn PTR (_, t) => (fn a => (a, t))
            | _ => bug "Heavy.ptr (non-object-pointer-type)"
        val fptr : 'f fptr T.typ  -> 'f fptr' -> 'f fptr =
           fn (FPTR mkf) => (fn a => (a, #2 (mkf a)))
            | _ => bug "Heavy.fptr (non-function-pointer-type)"
    end

    val sizeof : ('t, 'c) obj -> 't S.size =
       fn (_, t) => T.sizeof t

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

        val ptr : ('o ptr, 'c) obj -> 'o ptr =
           fn (a, PTR (_, t)) => (ptr' a, t)
            | _ => bug "Get.ptr (non-pointer)"
        val fptr : ('f, 'c) fptr_obj -> 'f fptr =
           fn (a, FPTR mkf) => let val fa = fptr' a in (fa, #2 (mkf fa)) end
            | _ => bug "Get.fptr (non-function-pointer)"

        local
            val u2s = MLRep.Int.Signed.fromLarge o MLRep.Int.Unsigned.toLargeIntX
        in
            fun ubf ({ a, l, r=_, lr, m=_, im=_ } : bf) =
                (CMemory.load_uint a << l) >> lr
            fun sbf ({ a, l, r=_, lr, m=_, im=_ } : bf) =
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
            val enum = enum' $ strip_type

            val ptr : ('o ptr, rw) obj * 'o ptr -> unit =
               fn (x, p) => ptr' (p_strip_type x, p_strip_type p)
            val voidptr = voidptr' $ strip_type
            val fptr : ('f, rw) fptr_obj * 'f fptr -> unit =
               fn (x, f) => fptr' (p_strip_type x, strip_fun f)

            val ptr_voidptr : ('o ptr, rw) obj * voidptr -> unit =
               fn (x, p) => ptr_voidptr' (p_strip_type x, p)
        end

        fun ubf ({ a, l=_, r, lr=_, m, im }, x) =
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
        val |&| : ('t, 'c) obj -> ('t, 'c) obj ptr = 
           fn (a, t) => (a, PTR (null, t))
        val |*| : ('t, 'c) obj ptr -> ('t, 'c) obj = 
           fn (a, PTR (_, t)) => (a, t)
            | _ => bug "Ptr.* (non-pointer)"

        val |&! : ('t, 'c) obj' -> ('t, 'c) obj ptr' = 
           addr_id
        val |*! : ('t, 'c) obj ptr' -> ('t, 'c) obj' = 
           addr_id

        fun compare (p, p') = CMemory.compare (p_strip_type p, p_strip_type p')

        val compare' = CMemory.compare

        val inject' = addr_id
        val cast' = addr_id

        val inject : 'o ptr -> voidptr = p_strip_type
        val cast : 'o ptr T.typ -> voidptr -> 'o ptr =
           fn PTR (_, t) => (fn p => (p, t))
            | _ => bug "Ptr.cast (non-pointer-type)"

        val vnull : voidptr = CMemory.null
        val null : 'o ptr T.typ -> 'o ptr =
           fn t => cast t vnull
        val null' : 'o ptr' = vnull

        val fnull' : 'f ptr' = CMemory.null
        val fnull : 'f fptr T.typ -> 'f fptr =
           fn t => Heavy.fptr t fnull'

        val isVNull : voidptr -> bool = CMemory.isNull
        val isNull : 'o ptr -> bool =
           fn p => isVNull (inject p)
        val isNull' = CMemory.isNull

        val isFNull : 'f fptr -> bool =
           fn (a,_) => CMemory.isNull a
        val isFNull' = CMemory.isNull

        fun |+! s (p, i) = p ++ (Word.toInt s * i)
        fun |-! s (p, p') = (p -- p') div Word.toInt s

        val |+| : ('t, 'c) obj ptr * int -> ('t, 'c) obj ptr =
           fn ((p, t as PTR (_, t')), i) => (|+! (T.sizeof t') (p, i), t)
            | _ => bug "Ptr.|+| (non-pointer-type)"
        val |-| : ('t, 'c) obj ptr * ('t, 'c) obj ptr -> int =
           fn ((p, PTR (_, t')), (p', _)) => |-! (T.sizeof t') (p, p')
            | _ => bug "Ptr.|-| (non-pointer-type"

        val sub : ('t, 'c) obj ptr * int -> ('t, 'c) obj =
           fn (p, i) => |*| (|+| (p, i))

        fun sub' t (p, i) = |*! (|+! t (p, i))

        val convert : (('st, 'sc) obj ptr, ('tt, 'tc) obj ptr) W.witness ->
                      ('st, 'sc) obj ptr -> ('tt, 'tc) obj ptr =
           fn w => fn x => w x
        val convert' : (('st, 'sc) obj ptr, ('tt, 'tc) obj ptr) W.witness ->
                       ('st, 'sc) obj ptr' -> ('tt, 'tc) obj ptr' =
           fn _ => fn x => x

        val ro : ('t, 'c) obj ptr   -> ('t, ro) obj ptr =
           fn x => convert (W.pointer (W.ro W.trivial)) x
        val rw : ('t, 'sc) obj ptr  -> ('t, 'tc) obj ptr =
           fn x => convert (W.pointer (W.rw W.trivial)) x

        val ro' : ('t, 'c) obj ptr'  -> ('t, ro) obj ptr' =
           addr_id
        val rw' : ('t, 'sc) obj ptr' -> ('t, 'tc) obj ptr' = 
           addr_id
    end

    structure Arr = struct
        local
            fun asub (a, i, n, esz) =
                (* take advantage of wrap-around to avoid the >= 0 test... *)
                if Word.fromInt i < n then a ++ (Word.toIntX esz * i)
                else raise General.Subscript
        in
            val sub : (('t, 'n) arr, 'c) obj * int -> ('t, 'c) obj =
               fn ((a, ARR { typ, n, esz, ... }), i) => (asub (a, i, n, esz), typ)
                | _ => bug "Arr.sub (non-array)"
            val sub' : 't S.size * 'n Dim.dim -> 
                        (('t, 'n) arr, 'c) obj' * int -> ('t, 'c) obj' =
               fn (s, d) => fn (a, i) => asub (a, i, Word.fromInt (Dim.toInt d), s)
        end

        val decay : (('t, 'n) arr, 'c) obj -> ('t, 'c) obj ptr =
           fn (a, ARR { typ, ... }) => (a, PTR (null, typ))
            | _ => bug "Arr.decay (non-array)"

        val decay' = addr_id

        val reconstruct : ('t, 'c) obj ptr * 'n Dim.dim -> (('t, 'n) arr, 'c) obj =
           fn ((a, PTR (_, t)), d) => (a, T.arr (t, d))
            | _ => bug "Arr.reconstruct (non-pointer)"

        fun reconstruct' (a: addr, _: 'n Dim.dim) = a

        fun dim (_: addr, t) = T.dim t
    end

    fun new' s = CMemory.alloc s
    val new : 't T.typ -> ('t, 'c) obj =
       fn t => (new' (T.sizeof t), t)

    val discard' = CMemory.free
    val discard : ('t, 'c) obj -> unit =
       fn x => discard' (p_strip_type x)

    fun alloc' s i = CMemory.alloc (s * i)
    val alloc : 't T.typ -> word -> ('t, 'c) obj ptr =
       fn t => fn i => (alloc' (T.sizeof t) i, PTR (null, t))

    val free' = CMemory.free
    val free : 'o ptr -> unit =
       fn x => free' (p_strip_type x)

    val call : ('a -> 'b) fptr * 'a -> 'b =
       fn ((_, f), x) => f x

    val call' : ('a -> 'b) fptr T.typ -> ('a -> 'b) fptr' * 'a -> 'b =
       fn (FPTR mkf) => (fn (a, x) => (#2 (mkf a)) x)
        | _ => bug "call' (non-function-pointer-type)"

    structure U = struct
        fun fcast (f : 'fa fptr') : 'fb fptr' = f
        fun p2i (a : 'o ptr') : ulong = CMemory.p2i a
        fun i2p (a : ulong) : 'o ptr' = CMemory.i2p a
    end

    (* ------------- internal stuff ------------- *)

    fun mk_obj' (a : addr) = a
    val mk_voidptr : addr -> voidptr = fn a => a

    local
        fun mk_field (t: 'f objt, i, (a, _)) = (a ++ i, t)
    in
        val mk_rw_field : 'm T.typ * int * ('s, 'c) su_obj -> ('m, 'c) obj = mk_field
        val mk_ro_field : 'm T.typ * int * ('s, 'c) su_obj -> ('m, ro) obj = mk_field
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
        fun mk_bf acc (a, _) = mk_bf' acc a
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

    val mk_su_size : word -> 's S.size =
       fn sz => sz
    val mk_su_typ : 's su S.size -> 's su T.typ =
       fn sz => BASE sz
    val mk_fptr : (addr -> 'a -> 'b) * addr -> ('a -> 'b) fptr =
       fn (mkf, a) => (a, mkf a)
    val mk_fptr_typ : (addr -> 'a -> 'b) -> ('a -> 'b) fptr T.typ =
       fn mkf => FPTR (fn a => (null, mkf a))

    val reveal : voidptr -> addr = addr_id
    val freveal : 'f fptr' -> addr = addr_id

    val vcast : addr -> voidptr = addr_id
    val pcast : addr -> 'o ptr' = addr_id
    val fcast : addr -> 'f fptr' = addr_id

    fun unsafe_sub esz (a, i) = a ++ esz * i
    end (* local *)
end
in
structure C_Int = C
end
