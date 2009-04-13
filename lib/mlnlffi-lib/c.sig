(* c.sig
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(*
 * Encoding the C type system in SML.
 *
 *   (C) 2001, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume
 *)
signature C = sig

    exception OutOfMemory

    (* objects of type 't, constness 'c;
     * The 't type variable will be instantiated with the object's "witness"
     * type. The intention is that there be an isomorphism between such
     * witness types and corresponding C types.
     *
     * Witness types are often the same as the (abstract) type of the value
     * stored in the object.  However, this is merely a coincidence.  For
     * example, a constant object holding a pointer to a read-write integer
     * would have type 
     *   ((sint, rw) ptr, ro) obj 
     * and the value itself has type 
     *   (sint, rw) ptr.
     *
     * However, in the case of the "light" version of this object (see below),
     * the object type is 
     *   ((sint, rw) ptr, ro) obj' 
     * while fetching from this object gives a value of type 
     *   (sint, rw) ptr'.  
     *
     * (In other words, we use the "heavy" versions of value types as witness
     * types -- even in the "light" case.) *)
    type ('t, 'c) obj

    (* an alternative "light-weight" version that does not carry RTTI at
     * the cost of requiring explicit passing of RTTI for certain operations *)
    eqtype ('t, 'c) obj'

    (* constness property, to be substituted for 'c *)
    type ro and rw

    (* Pointers come in two varieties in C:  Pointers to things we
     * know and pointers to "incomplete" types.  The "ptr" type constructor
     * below encodes both kinds using the following convention:
     *   - in the case of complete target types, 'o will be instantiated
     *     to some ('t, 'c) obj
     *   - in the case of incomplete target types, 'o will be instantiated
     *     to some fresh (abstract) type (see iptr.sig for what this will
     *     look like in practice)
     *)
    (* pointer to 'o *)
    type 'o ptr 
    (* light-weight alternative *)                      
    eqtype 'o ptr'

    (* 'n-sized array with 't elements *)
    type ('t, 'n) arr

    (* no values, admits equality *)
    eqtype void 

    (* void* is really a base type, but it happens to take the
     * form of a light-weight pointer type (with an abstract target).
     * This design makes it possible to use those ptr-related
     * functions that "make sense" for void*. *)
    (* C's void* *)
    type voidptr = void ptr'

    (* function pointers *)
    (* a function pointer *)
    type 'f fptr
    (* light-weight alternative *)
    eqtype 'f fptr'

    (* structures and unions *)
    (* struct/union named 'tag; 
     * 'tag is drawn from the types defined in the Tag module 
     *) 
    type 'tag su     

    (* enumerations *)
    eqtype 'tag enum

    (* primtypes (signed/unsigned char, short, int, long, long long; float, double) *)
    eqtype schar     and uchar
    eqtype sshort    and ushort 
    eqtype sint      and uint
    eqtype slong     and ulong
    eqtype slonglong and ulonglong
    type float       and double

    (* going from abstract to concrete and vice versa;
     * (this shouldn't be needed except when calling functions through
     * function pointers) *)
    structure Cvt : sig
        (* ML -> C *)
        val c_schar     : MLRep.Char.Signed.int        -> schar
        val c_uchar     : MLRep.Char.Unsigned.word     -> uchar
        val c_sshort    : MLRep.Short.Signed.int       -> sshort
        val c_ushort    : MLRep.Short.Unsigned.word    -> ushort
        val c_sint      : MLRep.Int.Signed.int         -> sint
        val c_uint      : MLRep.Int.Unsigned.word      -> uint
        val c_slong     : MLRep.Long.Signed.int        -> slong
        val c_ulong     : MLRep.Long.Unsigned.word     -> ulong
        val c_slonglong : MLRep.LongLong.Signed.int    -> slonglong
        val c_ulonglong : MLRep.LongLong.Unsigned.word -> ulonglong
        val c_float     : MLRep.Float.real             -> float
        val c_double    : MLRep.Double.real            -> double
        val i2c_enum    : MLRep.Int.Signed.int         -> 'e enum

        (* C -> ML *)
        val ml_schar     : schar      -> MLRep.Char.Signed.int
        val ml_uchar     : uchar      -> MLRep.Char.Unsigned.word
        val ml_sshort    : sshort     -> MLRep.Short.Signed.int
        val ml_ushort    : ushort     -> MLRep.Short.Unsigned.word
        val ml_sint      : sint       -> MLRep.Int.Signed.int
        val ml_uint      : uint       -> MLRep.Int.Unsigned.word
        val ml_slong     : slong      -> MLRep.Long.Signed.int
        val ml_ulong     : ulong      -> MLRep.Long.Unsigned.word
        val ml_slonglong : slonglong  -> MLRep.LongLong.Signed.int
        val ml_ulonglong : ulonglong  -> MLRep.LongLong.Unsigned.word
        val ml_float     : float      -> MLRep.Float.real
        val ml_double    : double     -> MLRep.Double.real
        val c2i_enum     : 'e enum    -> MLRep.Int.Signed.int
    end

    (* type-abbreviations for a bit more convenience. *)
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

    (* light-weight alternatives *)
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

    (* bitfields aren't "ordinary objects", so they have their own type *)
    eqtype 'c sbf and 'c ubf

    structure W : sig
        (* conversion "witness" values *)
        type ('from, 'to) witness

        (* A small calculus for generating new witnesses.
         * Since the only witness constructors that do anything real are
         * rw and ro, all this calculus gives you is a way of changing
         * "const" attributes at any level within a bigger type.
         *
         * (The calculus can express nonsensical witnesses -- which
         * fortunately are harmless because they can't be applied to any
         * values.) *)
        val trivial : ('t, 't) witness

        val pointer : ('from, 'to) witness ->
                      ('from ptr, 'to ptr) witness
        val object  : ('from, 'to) witness ->
                      (('from, 'c) obj, ('to, 'c) obj) witness
        val arr     : ('from, 'to) witness ->
                      (('from, 'n) arr, ('to, 'n) arr) witness

        val ro      : ('from, 'to) witness ->
                      (('from, 'fc) obj, ('to, ro) obj) witness
        val rw      : ('from, 'to) witness ->
                      (('from, 'fc) obj, ('to, 'tc) obj) witness
    end

    (* Object conversions that rely on witnesses: *)
    val convert : (('st, 'sc) obj, ('tt, 'tc) obj) W.witness ->
                  ('st, 'sc) obj -> ('tt, 'tc) obj
    val convert' : (('st, 'sc) obj, ('tt, 'tc) obj) W.witness ->
                   ('st, 'sc) obj' -> ('tt, 'tc) obj'

    (* 
     * A family of types and corresponding values representing natural numbers.
     *   (An encoding in SML without using dependent types.)
     *)
    structure Dim : sig

        (* Internally, a value of type ('a, 'z) dim0 is just a number.
         * The trick here is to give each of these numbers a different unique
         * type. 'a will be a decimal encoding of the number's value in
         * "type digits". 'z keeps track of whether the number is zero or not. 
         *)
        type ('a, 'z) dim0

        (* We can always get the internal number back... *)
        val toInt : ('a, 'z) dim0 -> int

        (* These two types act as "flags". They will be substituted for 'z
         * and remember whether the value is zero or not. *)
        type zero
        type nonzero

        type 'a dim = ('a, nonzero) dim0

        (* These are the "type digits".  Type "dec" acts as a "terminator".
         * We chose its name so to remind us that the encoding is decimal.
         * If a nonzero value's decimal representation is "<Dn>...<D0>", then
         * its type will be "(dec dg<Dn> ... dg<D0>, nonzero) dim0", which
         * is the same as "dec dg<Dn> ... dg<D0> dim".  The type of the zero
         * value is "(dec, zero) dim0". *)
        type dec
        type 'a dg0 and 'a dg1 and 'a dg2 and 'a dg3 and 'a dg4
        type 'a dg5 and 'a dg6 and 'a dg7 and 'a dg8 and 'a dg9

        (* Here are the corresponding constructors for ('a, 'z) dim0 values.
         * The type for dg0 ensures that there will be no "leading zeros" in
         * any encoding.  This guarantees a 1-1 correspondence of constructable
         * values and their types.
         * To construct the value corresponding to a nonzero number whose
         * decimal representation is "<Dn>...<D0>", one must invoke
         * "dg<D0>' (... (dg<Dn>' dec')...)", i.e., the least significant
         * digit appears leftmost. *)
        val dec' :            (dec, zero) dim0
        val dg0' : 'a dim        -> 'a dg0 dim
        val dg1' : ('a, 'z) dim0 -> 'a dg1 dim
        val dg2' : ('a, 'z) dim0 -> 'a dg2 dim
        val dg3' : ('a, 'z) dim0 -> 'a dg3 dim
        val dg4' : ('a, 'z) dim0 -> 'a dg4 dim
        val dg5' : ('a, 'z) dim0 -> 'a dg5 dim
        val dg6' : ('a, 'z) dim0 -> 'a dg6 dim
        val dg7' : ('a, 'z) dim0 -> 'a dg7 dim
        val dg8' : ('a, 'z) dim0 -> 'a dg8 dim
        val dg9' : ('a, 'z) dim0 -> 'a dg9 dim

        (* Since having to reverse the sequence of digits seems unnatural,
         * here is another set of constructors for dim values.  These
         * constructors use continuation-passing style and themselves
         * have more complicated types.  But their use is easier:
         * To construct the value corresponding to a nonzero number whose
         * decimal representation is "<Dn>...<D0>", one must invoke
         * "dec dg<Dn> ... dg<D0> dim"; i.e., the least significant
         * digit appears rightmost -- just like in the usual decimal
         * notation for numbers that we are all familiar with.
         * [Moreover, for any 'a dim value we have the neat property that
         * it can be constructed by taking its type (expressed using "dim")
         * and interpreting it as an expression.  For example, the dim
         * value representing 312 is "dec dg3 dg1 dg2 dim" and it can
         * be constructed by evaluating "dec dg3 dg1 dg2 dim".] *)
        val dec :            ((dec, zero) dim0 -> 'b) -> 'b

        val dg0 : 'a dim        -> ('a dg0 dim -> 'b) -> 'b
        val dg1 : ('a, 'z) dim0 -> ('a dg1 dim -> 'b) -> 'b
        val dg2 : ('a, 'z) dim0 -> ('a dg2 dim -> 'b) -> 'b
        val dg3 : ('a, 'z) dim0 -> ('a dg3 dim -> 'b) -> 'b
        val dg4 : ('a, 'z) dim0 -> ('a dg4 dim -> 'b) -> 'b
        val dg5 : ('a, 'z) dim0 -> ('a dg5 dim -> 'b) -> 'b
        val dg6 : ('a, 'z) dim0 -> ('a dg6 dim -> 'b) -> 'b
        val dg7 : ('a, 'z) dim0 -> ('a dg7 dim -> 'b) -> 'b
        val dg8 : ('a, 'z) dim0 -> ('a dg8 dim -> 'b) -> 'b
        val dg9 : ('a, 'z) dim0 -> ('a dg9 dim -> 'b) -> 'b

        val dim : ('a, 'z) dim0 -> ('a, 'z) dim0
    end

    (* sub-structure for dealing with run-time type info (sizes only) *)
    structure S : sig

        (* Our size info itself is statically typed!
         * The size info for a value stored in ('t, 'c) obj has
         * the following type: *)
        type 't size

        (* get a number out *)
        val toWord : 't size -> word

        (* sizes for simple things *)
        val schar     : schar size
        val uchar     : uchar size
        val sshort    : sshort size
        val ushort    : ushort size
        val sint      : sint size
        val uint      : uint size
        val slong     : slong size
        val ulong     : ulong size
        val slonglong : slonglong size
        val ulonglong : ulonglong size
        val float     : float size
        val double    : double size

        val voidptr : voidptr size
        val ptr : 'o ptr size
        val fptr : 'f fptr size
        val enum : 'tag enum size
    end

    (* sub-structure for dealing with run-time type info *)
    structure T : sig

        (* Our RTTI itself is statically typed!
         * The RTTI for a value stored in ('t, 'c) obj has
         * the following type: *)
        type 't typ

        (* get the RTTI from an actual object *)
        val typeof : ('t, 'c) obj -> 't typ

        (* constructing new RTTI from existing RTTI *)
        val pointer : 't typ -> ('t, rw) obj ptr typ
        val target  : ('t, 'c) obj ptr typ -> 't typ
        val arr     : 't typ * 'n Dim.dim -> ('t, 'n) arr typ
        val elem    : ('t, 'n) arr typ -> 't typ
        val ro      : ('t, 'c) obj ptr typ -> ('t, ro) obj ptr typ

        (* calculating the size of an object given its RTTI *)
        val sizeof : 't typ -> 't S.size

        (* dimension of array type *)
        val dim : ('t, 'n) arr typ -> 'n Dim.dim

        (* RTTI for simple things *)
        val schar     :     schar typ
        val uchar     :     uchar typ
        val sshort    :    sshort typ
        val ushort    :    ushort typ
        val sint      :      sint typ
        val uint      :      uint typ
        val slong     :     slong typ
        val ulong     :     ulong typ
        val slonglong : slonglong typ
        val ulonglong : ulonglong typ
        val float     :     float typ
        val double    :    double typ

        val voidptr : voidptr typ

        val enum : 'tag enum typ
    end

    (* convert from regular (heavy) to alternative (light) versions *)
    structure Light : sig
        val obj : ('t, 'c) obj -> ('t, 'c) obj'
        val ptr : 'o ptr -> 'o ptr'
        val fptr : 'f fptr -> 'f fptr'
    end

    (* and vice versa *)
    structure Heavy : sig
        val obj : 't T.typ -> ('t, 'c) obj' -> ('t, 'c) obj
        val ptr : 'o ptr T.typ -> 'o ptr' -> 'o ptr
        val fptr : 'f fptr T.typ  -> 'f fptr' -> 'f fptr
    end

    (* calculate size of an object *)
    val sizeof : ('t, 'c) obj -> 't S.size

    (* "fetch" methods for various types;
     * fetching does not care about constness *)
    structure Get : sig
        (* primitive types; the results are concrete here *)
        val schar     : 'c schar_obj      -> MLRep.Char.Signed.int
        val uchar     : 'c uchar_obj      -> MLRep.Char.Unsigned.word
        val sshort    : 'c sshort_obj     -> MLRep.Short.Signed.int
        val ushort    : 'c ushort_obj     -> MLRep.Short.Unsigned.word
        val sint      : 'c sint_obj       -> MLRep.Int.Signed.int
        val uint      : 'c uint_obj       -> MLRep.Int.Unsigned.word
        val slong     : 'c slong_obj      -> MLRep.Long.Signed.int
        val ulong     : 'c ulong_obj      -> MLRep.Long.Unsigned.word
        val slonglong : 'c slonglong_obj  -> MLRep.LongLong.Signed.int
        val ulonglong : 'c ulonglong_obj  -> MLRep.LongLong.Unsigned.word
        val float     : 'c float_obj      -> MLRep.Float.real
        val double    : 'c double_obj     -> MLRep.Double.real
        val enum      : ('e, 'c) enum_obj -> MLRep.Int.Signed.int

        (* alt *)
        val schar'     : 'c schar_obj'      -> MLRep.Char.Signed.int
        val uchar'     : 'c uchar_obj'      -> MLRep.Char.Unsigned.word
        val sshort'    : 'c sshort_obj'     -> MLRep.Short.Signed.int
        val ushort'    : 'c ushort_obj'     -> MLRep.Short.Unsigned.word
        val sint'      : 'c sint_obj'       -> MLRep.Int.Signed.int
        val uint'      : 'c uint_obj'       -> MLRep.Int.Unsigned.word
        val slong'     : 'c slong_obj'      -> MLRep.Long.Signed.int
        val ulong'     : 'c ulong_obj'      -> MLRep.Long.Unsigned.word
        val slonglong' : 'c slonglong_obj'  -> MLRep.LongLong.Signed.int
        val ulonglong' : 'c ulonglong_obj'  -> MLRep.LongLong.Unsigned.word
        val float'     : 'c float_obj'      -> MLRep.Float.real
        val double'    : 'c double_obj'     -> MLRep.Double.real
        val enum'      : ('e, 'c) enum_obj' -> MLRep.Int.Signed.int

        (* fetching pointers; results have to be abstract *)
        val ptr : ('o ptr, 'c) obj -> 'o ptr
        val fptr : ('f, 'c) fptr_obj -> 'f fptr
        val voidptr : 'c voidptr_obj -> voidptr

        (* alt *)
        val ptr' : ('o ptr, 'c) obj' -> 'o ptr'
        val fptr' : ('f, 'c) fptr_obj' -> 'f fptr'
        val voidptr' : 'c voidptr_obj' -> voidptr

        (* bitfields; concrete again *)
        val sbf : 'c sbf -> MLRep.Int.Signed.int
        val ubf : 'c ubf -> MLRep.Int.Unsigned.word
    end

    (* "store" methods; these require rw objects *)
    structure Set : sig
        (* primitive types; use concrete values *)
        val schar     : rw schar_obj      * MLRep.Char.Signed.int        -> unit
        val uchar     : rw uchar_obj      * MLRep.Char.Unsigned.word     -> unit
        val sshort    : rw sshort_obj     * MLRep.Short.Signed.int       -> unit
        val ushort    : rw ushort_obj     * MLRep.Short.Unsigned.word    -> unit
        val sint      : rw sint_obj       * MLRep.Int.Signed.int         -> unit
        val uint      : rw uint_obj       * MLRep.Int.Unsigned.word      -> unit
        val slong     : rw slong_obj      * MLRep.Long.Signed.int        -> unit
        val ulong     : rw ulong_obj      * MLRep.Long.Unsigned.word     -> unit
        val slonglong : rw slonglong_obj  * MLRep.LongLong.Signed.int    -> unit
        val ulonglong : rw ulonglong_obj  * MLRep.LongLong.Unsigned.word -> unit
        val float     : rw float_obj      * MLRep.Float.real             -> unit
        val double    : rw double_obj     * MLRep.Double.real            -> unit
        val enum      : ('e, rw) enum_obj * MLRep.Int.Signed.int         -> unit

        (* alt *)
        val schar'     : rw schar_obj'      * MLRep.Char.Signed.int        -> unit
        val uchar'     : rw uchar_obj'      * MLRep.Char.Unsigned.word     -> unit
        val sshort'    : rw sshort_obj'     * MLRep.Short.Signed.int       -> unit
        val ushort'    : rw ushort_obj'     * MLRep.Short.Unsigned.word    -> unit
        val sint'      : rw sint_obj'       * MLRep.Int.Signed.int         -> unit
        val uint'      : rw uint_obj'       * MLRep.Int.Unsigned.word      -> unit
        val slong'     : rw slong_obj'      * MLRep.Long.Signed.int        -> unit
        val ulong'     : rw ulong_obj'      * MLRep.Long.Unsigned.word     -> unit
        val slonglong' : rw slonglong_obj'  * MLRep.LongLong.Signed.int    -> unit
        val ulonglong' : rw ulonglong_obj'  * MLRep.LongLong.Unsigned.word -> unit
        val float'     : rw float_obj'      * MLRep.Float.real             -> unit
        val double'    : rw double_obj'     * MLRep.Double.real            -> unit
        val enum'      : ('e, rw) enum_obj' * MLRep.Int.Signed.int         -> unit

        (* storing pointers; abstract *)
        val ptr : ('o ptr, rw) obj * 'o ptr -> unit
        val fptr : ('f, rw) fptr_obj * 'f fptr -> unit
        val voidptr : rw voidptr_obj * voidptr -> unit

        (* alt *)
        val ptr' : ('o ptr, rw) obj' * 'o ptr' -> unit
        val fptr' : ('f, rw) fptr_obj' * 'f fptr' -> unit
        val voidptr' : rw voidptr_obj' * voidptr -> unit

        (* When storing, voidptr is compatible with any ptr type
         * (just like in C).  This should eliminate most need for RTTI in
         * practice. *)
        val ptr_voidptr : ('o ptr, rw) obj * voidptr -> unit

        (* alt *)
        val ptr_voidptr' : ('o ptr, rw) obj' * voidptr -> unit

        (* bitfields; concrete *)
        val sbf : rw sbf * MLRep.Int.Signed.int    -> unit
        val ubf : rw ubf * MLRep.Int.Unsigned.word -> unit
    end

    (* copying the contents of arbitrary objects *)
    val copy : { from: ('t, 'c) obj, to: ('t, rw) obj } -> unit

    (* alt *)
    val copy' : 't S.size -> { from: ('t, 'c) obj', to: ('t, rw) obj' } -> unit

    (* manipulating object constness
     * rw -> ro:  this direction just accounts for the fact that
     *            rw is conceptually a subtype of ro
     * ro -> rw:  this is not safe, but C makes it so easy that we
     *            might as well directly support it;
     * Concretely, we make both operations polymorphic in the argument
     * constness.  Moreover, the second (unsafe) direction is also
     * polymorphic in the result.  This can be used to effectively
     * implement a conversion to "whatever the context wants".
     *
     * Note: fun ro x = convert (W.ro W.trivial) x
     *       etc.
     *)
    val ro : ('t, 'c) obj  -> ('t, ro) obj
    val rw : ('t, 'sc) obj -> ('t, 'tc) obj

    (* alt *)
    val ro' : ('t, 'c) obj'  -> ('t, ro) obj'
    val rw' : ('t, 'sc) obj' -> ('t, 'tc) obj'

    (* operations on (mostly) pointers *)
    structure Ptr : sig

        (* going from object to pointer and vice versa *)
        val |&| : ('t, 'c) obj -> ('t, 'c) obj ptr
        val |*| : ('t, 'c) obj ptr -> ('t, 'c) obj

        (* alt *)
        val |&! : ('t, 'c) obj' -> ('t, 'c) obj ptr'
        val |*! : ('t, 'c) obj ptr' -> ('t, 'c) obj'

        (* comparing pointers *)
        val compare : 'o ptr * 'o ptr -> order

        (* alt *)
        val compare' : 'o ptr' * 'o ptr' -> order

        (* going from pointer to void*;  this also accounts for a conceptual
         * subtyping relation and is safe *)
        val inject : 'o ptr -> voidptr

        (* alt *)
        val inject' : 'o ptr' -> voidptr

        (* the opposite is not safe, but C makes it not only easy but also
         * almost necessary; we use our RTTI interface to specify the pointer
         * type (not the element type!) *)
        val cast : 'o ptr T.typ -> voidptr -> 'o ptr

        (* alt, needs explicit type constraint on result! *)
        val cast' : voidptr -> 'o ptr'

        (* NULL as void* *)
        val vnull : voidptr

        (* projecting vnull to given pointer type *)
        val null : 'o ptr T.typ -> 'o ptr

        (* the "light" NULL pointer is simply a polymorphic constant *)
        val null' : 'o ptr'

        (* fptr version of NULL *)
        val fnull : 'f fptr T.typ -> 'f fptr

        (* again, "light" version is simply a polymorphic constant *)
        val fnull' : 'f fptr'

        (* checking for NULL pointer *)
        val isVNull : voidptr -> bool

        (* combining inject and isVNull for convenience *)
        val isNull : 'o ptr -> bool

        (* alt *)
        val isNull' : 'o ptr' -> bool

        (* checking a function pointer for NULL *)
        val isFNull : 'f fptr -> bool

        (* alt *)
        val isFNull' : 'f fptr' -> bool

        (* pointer arithmetic *)
        val |+| : ('t, 'c) obj ptr * int -> ('t, 'c) obj ptr
        val |-| : ('t, 'c) obj ptr * ('t, 'c) obj ptr -> int

        (* alt; needs explicit size (for element) *)
        val |+! : 't S.size -> ('t, 'c) obj ptr' * int -> ('t, 'c) obj ptr'
        val |-! : 't S.size -> ('t, 'c) obj ptr' * ('t, 'c) obj ptr' -> int

        (* subscript through a pointer; this is unchecked *)
        val sub : ('t, 'c) obj ptr * int -> ('t, 'c) obj

        (* alt; needs explicit size (for element) *)
        val sub' : 't S.size -> ('t, 'c) obj ptr' * int -> ('t, 'c) obj'

        (* conversions that rely on witnesses *)
        val convert : (('st, 'sc) obj ptr, ('tt, 'tc) obj ptr) W.witness ->
                      ('st, 'sc) obj ptr -> ('tt, 'tc) obj ptr
        val convert' : (('st, 'sc) obj ptr, ('tt, 'tc) obj ptr) W.witness ->
                       ('st, 'sc) obj ptr' -> ('tt, 'tc) obj ptr'

        (* constness manipulation for pointers
         * Note: fun ro x = convert (W.pointer (W.ro W.trivial)) x
         *       etc. *)
        val ro  : ('t, 'c) obj ptr   -> ('t, ro) obj ptr
        val rw  : ('t, 'sc) obj ptr  -> ('t, 'tc) obj ptr
        val ro' : ('t, 'c) obj ptr'  -> ('t, ro) obj ptr'
        val rw' : ('t, 'sc) obj ptr' -> ('t, 'tc) obj ptr'
    end

    (* operations on (mostly) arrays *)
    structure Arr : sig

        (* array subscript;
         * since we have RTTI, we can actually make this safe:  we raise
         * General.Subscript for out-of-bounds access;
         * for unchecked access, go through arr_decay and ptr_sub 
         *)
        val sub : (('t, 'n) arr, 'c) obj * int -> ('t, 'c) obj

        (* alt; needs element size and array dimension *)
        val sub' : 't S.size * 'n Dim.dim ->
                   (('t, 'n) arr, 'c) obj' * int -> ('t, 'c) obj'

        (* let an array object decay, yielding pointer to first element *)
        val decay : (('t, 'n) arr, 'c) obj -> ('t, 'c) obj ptr

        (* alt *)
        val decay' : (('t, 'n) arr, 'c) obj' -> ('t, 'c) obj ptr'

        (* reconstruct an array object from the pointer to its first element *)
        val reconstruct :
            ('t, 'c) obj ptr * 'n Dim.dim -> (('t, 'n) arr, 'c) obj

        (* alt *)
        val reconstruct':
            ('t, 'c) obj ptr' * 'n Dim.dim -> (('t, 'n) arr, 'c) obj'

        (* dimension of array object *)
        val dim : (('t, 'n) arr, 'c) obj -> 'n Dim.dim
    end

    (* allocating new objects *)
    val new : 't T.typ -> ('t, 'c) obj

    (* alt *)
    val new' : 't S.size -> ('t, 'c) obj'

    (* freeing objects that were allocated earlier *)
    val discard : ('t, 'c) obj -> unit

    (* alt *)
    val discard' : ('t, 'c) obj' -> unit

    (* allocating a dynamically-sized array *)
    val alloc : 't T.typ -> word -> ('t, 'c) obj ptr

    (* alt *)
    val alloc' : 't S.size -> word -> ('t, 'c) obj ptr'

    (* freeing through pointers *)
    val free : 'o ptr -> unit

    (* alt *)
    val free' : 'o ptr' -> unit

    (* perform function call through function-pointer *)
    val call : ('a -> 'b) fptr * 'a -> 'b

    (* alt; needs explicit type for the function pointer *)
    val call' : ('a -> 'b) fptr T.typ -> ('a -> 'b) fptr' * 'a -> 'b

    (* completely unsafe stuff that every C programmer just *loves* to do *)
    structure U : sig
        val fcast : 'a fptr' -> 'b fptr'
        val p2i : 'o ptr' -> ulong
        val i2p : ulong -> 'o ptr'
    end
end
