/* Copyright (C) 2005-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
Consider the following schemes for representing object pointers and
mapping them to 64-bit native pointers.

A. 32 bits, with bottom two bits zero.
B. 32 bits, with bottom bit zero, shift left by one.
C. 32 bits, with bottom bit zero, shift left by two.
D. 32 bits, shift left by two.
E. 32 bits, shift left by three.
F. 40 bits, with bottom two bits zero.
G. 64 bits, with bottom two bits zero.

These schemes vary in the number of bits to represent a pointer in an
object, the time to load a pointer from memory into a register, the
amount of addressable memory, and the object alignment.

        bits    time    mem(G)  align
A       32      fast      4     4
B       32      slow      8     4
C       32      slow     16     8
D       32      slow     16     4
E       32      slow     32     8
F       40      slow    256     4
G       64      fast     4G     4

Each of the (A-F) has a variant (AX-FX) in which pointers are added to
some constant base address.  This gives access to any region in the
virtual address space instead of just the low addresses.

The following diagram demonstrates what portion of the native pointer
to which the object pointer corresponds.

64                              32                              0
|                               |                               |
-----------------------------------------------------------------

                                 ==============================00   A

                                ===============================0    B

                               ===============================0     C

                               ================================     D

                             =================================      E

                         ======================================00   F

 ==============================================================00   G

Algorithmically, we can compute the native pointer (P) from the object
pointer (O) (with bitsize Z), given a shift (S) and a base (B):

P = %add64(%shl64(%zxZ_64(O),S),B)

Likewise, we can compute the object pointer (O) from the native
pointer (P), given a shift (S) and a base (B):

O = %lobits64_Z(%shr64(%sub64(P,B),S))

Hence, each of the schemes may be characterized by the size Z of the
object pointer, the shift S, and whether or not the base B is zero.
Noting that
 %zx64_64(x) = x
 %shl64(x, 0) = x
 %add64(x, 0) = x
 %lobits64_64(x) = x
 %shr64(x, 0) = x
 %sub64(x, 0) = x
it is easy to compute the number of ALU operations required by each
scheme:

A  :: Z = 32, S == 0, B == 0   ops = 1
AX :: Z = 32, S == 0, B != 0   ops = 2
B  :: Z = 32, S == 1, B == 0   ops = 2
BX :: Z = 32, S == 1, B != 0   ops = 3
C  :: Z = 32, S == 2, B == 0   ops = 2
CX :: Z = 32, S == 2, B != 0   ops = 3
D  :: Z = 32, S == 2, B == 0   ops = 2
DX :: Z = 32, S == 2, B != 0   ops = 3
E  :: Z = 32, S == 3, B == 0   ops = 2
EX :: Z = 32, S == 3, B != 0   ops = 3
F  :: Z = 40, S == 0, B == 0   ops = 1 (#)
FX :: Z = 40, S == 0, B != 0   ops = 2 (#)
G  :: Z = 64, S == 0, B == 0   ops = 0

#: In schemes F and FX, the conversion from object pointer to native
pointer requires logical-shift-right, rather than zero-extend, since
the object pointer would be fetched from memory as a 64-bit value.
The cost may actually be higher, as storing an object pointer in
memory requires some care so as not to overwrite neighboring data.

It is not clear that any of the thirteen schemes dominates another.
Here are some thoughts.

(A) This is is what we have now, but is still useful on 64-bit
machines where the bottom 4G may be less cluttered than on a 32-bit
machine.

(AX) seems like a nice cost/benefit tradeoff for a program that only
needs 4G of memory, since the base can be used to find a contiguous 4G
somewhere in the address space.

(B) and (C) are similar, the tradeoff being to increase object
alignment requirements in order to allow more memory.  Importantly,
pointers having a bottom zero bit means that we can still set the
bottom bit to one to represent small values in sum types.

(D) and (E) are problematic because they leave no room to represent 
small objects in sum types with pointers.  I think that really rules
them out. 

(F) costs some in object alignment because a sequence of pointers in
an object may have to be padded to meet 4-byte alignment.  Loading a
pointer from memory into a register may be slightly faster than in
(B) or (C) because we don't have to shift, but I wonder if that
matters.

(G) costs the most in space, but has the fastest load time for
pointers of the schemes that allow access to 4G of memory.

A reasonable tradeoff in implementation complexity vs allowing our
users enough flexibility might be to provide:

        A, AX, B, BX, C, CX, G

After some experiments on those, we might be able to find a more
manageable set for users.
*/

#if defined (GC_MODEL_NATIVE32)
#define GC_MODEL_OBJPTR_SIZE 32
#define GC_MODEL_OBJPTR_SHIFT 0
#define GC_MODEL_OBJPTR_BASE 0
#define GC_MODEL_HEADER_SIZE 32
#define GC_MODEL_ARRLEN_SIZE 32
#elif defined (GC_MODEL_NATIVE64)
#define GC_MODEL_OBJPTR_SIZE 64
#define GC_MODEL_OBJPTR_SHIFT 0
#define GC_MODEL_OBJPTR_BASE 0
#define GC_MODEL_HEADER_SIZE 64
#define GC_MODEL_ARRLEN_SIZE 64
#else
#error GC_MODEL_* unspecified
#endif

#define GC_MODEL_MINALIGN_SHIFT max(2, GC_MODEL_OBJPTR_SHIFT + 1)
#define GC_MODEL_MINALIGN TWOPOWER(GC_MODEL_MINALIGN_SHIFT)

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */
