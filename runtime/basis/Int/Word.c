#include "platform.h"

/*
 * We have to be very careful implementing WordS_quot and WordS_rem using / and %
 * because C allows
 *  "The direction of truncation for / and the sign of the result for % are
 *   machine-dependent for negative operands, ..." (K&R p. 41) (See also p. 205.)
 * On the other hand, the SML Basis library spec is completely nailed down.
 * On x86, gcc implements / and % using idiv, which fortunately does have the
 * same semantics as the SML Basis library.  However, gcc's optimizer sometimes
 * takes advantage of the flexibility in the C spec when one of the arguments
 * is a constant, producing incorrect results.  So, we have two options:
 *
 * Put them in a separate file, all by themselves, without a static inline, and
 * use / and % where we know gcc's optimer can't hurt us.
 * OR
 * Use inline assembler.
 *
 * We've gone for the first option because of simplicity, and because
 * quot and rem are only used with the C codegen.  If you really want
 * speed, you could try inline assembler.
 *
 * To get this working on another architecture, you need to check how gcc
 * implements / and %.
 */

#if ! (defined (__amd64__) || defined (__hppa__) || defined (__i386__) || defined (__ppc__) || defined (__powerpc__) || defined (__sparc__))
#error check that C {/,%} correctly implement {quot,rem} from the basis library
#endif

#define coerce(f, t)                            \
        t f##_to##t (f x);                      \
        t f##_to##t (f x) {                     \
                return (t)x;                    \
        }

#define bothCoerce(from, to)                    \
        coerce (Word##S##from, Word##to)        \
        coerce (Word##U##from, Word##to)

#define WordS8_max (WordS8)INT8_MAX
#define WordS8_min (WordS8)INT8_MIN
#define WordS16_max (WordS16)INT16_MAX
#define WordS16_min (WordS16)INT16_MIN
#define WordS32_max (WordS32)INT32_MAX
#define WordS32_min (WordS32)INT32_MIN
#define WordS64_max (WordS64)INT64_MAX
#define WordS64_min (WordS64)INT64_MIN
#define WordU8_max (WordU8)UINT8_MAX
#define WordU16_max (WordU16)UINT16_MAX
#define WordU32_max (WordU32)UINT32_MAX
#define WordU64_max (WordU64)UINT64_MAX

#define binary(kind, name, op)                                          \
        Word##kind Word##kind##_##name (Word##kind w1, Word##kind w2);  \
        Word##kind Word##kind##_##name (Word##kind w1, Word##kind w2) { \
                return w1 op w2;                                        \
        }

#define bothBinary(size, name, op)              \
        binary (S##size, name, op)              \
        binary (U##size, name, op)

#define SaddCheckOverflows(size)                                        \
        Bool WordS##size##_addCheckOverflows (WordS##size x, WordS##size y) {   \
                if (x >= 0) {                                           \
                        if (y > WordS##size##_max - x)                  \
                                return TRUE;                            \
                } else if (y < WordS##size##_min - x)                   \
                        return TRUE;                                    \
                return FALSE;                                           \
        }

#define UaddCheckOverflows(size)                                        \
        Bool WordU##size##_addCheckOverflows (WordU##size x, WordU##size y) {   \
                if (y > WordU##size##_max - x)                          \
                        return TRUE;                                    \
                return FALSE;                                           \
        }

#define SmulCheckOverflows(size)                                        \
        Bool WordS##size##_mulCheckOverflows (WordS##size x, WordS##size y) {   \
                if ((x == (WordS##size)0) or (y == (WordS##size)0))     \
                        return FALSE;                                   \
                if (x > (WordS##size)0) {                               \
                        if (y > (WordS##size)0) {                       \
                                if (x > WordS##size##_quot (WordS##size##_max, y))      \
                                        return TRUE;                    \
                                return FALSE;                           \
                        } else /* (y < (WordS##size)0) */ {             \
                                if (y < WordS##size##_quot (WordS##size##_min, x))      \
                                        return TRUE;                    \
                                return FALSE;                           \
                        }                                               \
                } else /* (x < (WordS##size)0) */ {                     \
                        if (y > (WordS##size)0) {                       \
                                if (x < WordS##size##_quot (WordS##size##_min, y))      \
                                        return TRUE;                    \
                                return FALSE;                           \
                        } else /* (y < (WordS##size)0) */ {             \
                                if (y < WordS##size##_quot (WordS##size##_max, x))      \
                                        return TRUE;                    \
                                return FALSE;                           \
                        }                                               \
                }                                                       \
        }

#define negCheckOverflows(size)                                         \
        Bool Word##size##_negCheckOverflows (WordS##size x) {           \
                if (x == WordS##size##_min)                             \
                        return TRUE;                                    \
                return FALSE;                                           \
        }

#define SsubCheckOverflows(size)                                        \
        Bool WordS##size##_subCheckOverflows (WordS##size x, WordS##size y) {   \
                if (x >= 0) {                                           \
                        if (y < x - WordS##size##_max)                  \
                                return TRUE;                            \
                } else if (y > x - WordS##size##_min)                   \
                        return TRUE;                                    \
                return FALSE;                                           \
        }

#define compare(kind, name, op)                                         \
        Bool Word##kind##_##name (Word##kind w1, Word##kind w2);        \
        Bool Word##kind##_##name (Word##kind w1, Word##kind w2) {       \
                return w1 op w2;                                        \
        }

#define bothCompare(size, name, op)             \
        compare (S##size, name, op)             \
        compare (U##size, name, op)

#define unary(kind,name, op)                            \
        Word##kind Word##kind##_##name (Word##kind w);  \
        Word##kind Word##kind##_##name (Word##kind w) { \
                return op w;                            \
        }

#define shift(kind, name, op)                                           \
        Word##kind Word##kind##_##name (Word##kind w1, Word32 w2);      \
        Word##kind Word##kind##_##name (Word##kind w1, Word32 w2) {     \
                return w1 op w2;                                        \
        }

#define all(size)                                               \
        binary (size, add, +)                                   \
        SaddCheckOverflows (size)                               \
        UaddCheckOverflows (size)                               \
        binary (size, andb, &)                                  \
        compare (size, equal, ==)                               \
        bothCompare (size, ge, >=)                              \
        bothCompare (size, gt, >)                               \
        bothCompare (size, le, <=)                              \
        shift (size, lshift, <<)                                \
        bothCompare (size, lt, <)                               \
        bothBinary (size, mul, *)                               \
        unary (size, neg, -)                                    \
        negCheckOverflows (size)                                \
        unary (size, notb, ~)                                   \
        binary (size, orb, |)                                   \
        bothBinary (size, quot, /)                              \
        SmulCheckOverflows (size)                               \
        bothBinary (size, rem, %)                               \
        Word##size Word##size##_rol (Word##size w1, Word32 w2); \
        Word##size Word##size##_rol (Word##size w1, Word32 w2) {\
                return (w1 >> (size - w2)) | (w1 << w2);        \
        }                                                       \
        Word##size Word##size##_ror (Word##size w1, Word32 w2); \
        Word##size Word##size##_ror (Word##size w1, Word32 w2) {\
                return (w1 >> w2) | (w1 << (size - w2));        \
        }                                                       \
        shift (S##size, rshift, >>)                             \
        shift (U##size, rshift, >>)                             \
        binary (size, sub, -)                                   \
        SsubCheckOverflows (size)                               \
        binary (size, xorb, ^)                                  \
        bothCoerce (size, 64)                                   \
        bothCoerce (size, 32)                                   \
        bothCoerce (size, 16)                                   \
        bothCoerce (size, 8)

all (8)
all (16)
all (32)
all (64)

#undef coerce
#undef bothCoerce
#undef binary
#undef bothBinary
#undef SaddCheckOverflows
#undef UaddCheckOverflows
#undef SmulCheckOverflows
#undef negCheckOverflows
#undef SsubCheckOverflows
#undef compare
#undef bothCompare
#undef unary
#undef shift
#undef all
