
#ifndef MLTON_CODEGEN_WORDSQUOTREM
#define MLTON_CODEGEN_WORDSQUOTREM(func) func
#endif

#define binary(kind, name, op)                                          \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Word##kind Word##kind##_##name (Word##kind w1, Word##kind w2) {       \
    return w1 op w2;                                                    \
  }

#define bothBinary(size, name, op)              \
binary (S##size, name, op)                      \
binary (U##size, name, op)

#define compare(kind, name, op)                                         \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool Word##kind##_##name (Word##kind w1, Word##kind w2) {             \
    return w1 op w2;                                                    \
  }

#define bothCompare(size, name, op)             \
compare (S##size, name, op)                     \
compare (U##size, name, op)

#define rol(size)                                                       \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Word##size Word##size##_rol (Word##size w1, Word32 w2) {              \
    return (w1 >> (size - w2)) | (w1 << w2);                            \
  }

#define ror(size)                                                       \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Word##size Word##size##_ror (Word##size w1, Word32 w2) {              \
    return (w1 >> w2) | (w1 << (size - w2));                            \
  }                                                                     \
  
#define shift(kind, name, op)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Word##kind Word##kind##_##name (Word##kind w1, Word32 w2) {           \
    return w1 op w2;                                                    \
  }

#define unary(kind,name, op)                                            \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Word##kind Word##kind##_##name (Word##kind w) {                       \
    return op w;                                                        \
  }

#define all(size)                               \
binary (size, add, +)                           \
binary (size, andb, &)                          \
compare (size, equal, ==)                       \
bothCompare (size, ge, >=)                      \
bothCompare (size, gt, >)                       \
bothCompare (size, le, <=)                      \
shift (size, lshift, <<)                        \
bothCompare (size, lt, <)                       \
bothBinary (size, mul, *)                       \
unary (size, neg, -)                            \
unary (size, notb, ~)                           \
/* WordS<N>_quot and WordS<N>_rem can't be inlined with the C-codegen,  \ 
 * because the gcc optimizer sometimes produces incorrect results       \
 * when one of the arguments is a constant.                             \
 * WordS<N>_quot and WordS<N>_rem can be inlined with the               \
 * bytecode-codegen, since they will be used in a context where the     \
 * arguments are variables.                                             \
 */                                                                     \
MLTON_CODEGEN_WORDSQUOTREM(binary (S##size, quot, /))                   \
MLTON_CODEGEN_WORDSQUOTREM(binary (S##size, rem, %))                    \
binary (U##size, quot, /)                       \
binary (U##size, rem, %)                        \
binary (size, orb, |)                           \
rol(size)                                       \
ror(size)                                       \
/* WordS<N>_rshift isn't ANSI C, because ANSI doesn't guarantee sign    \
 * extension.  We use it anyway cause it always seems to work.          \
 */                                                                     \
shift (S##size, rshift, >>)                     \
shift (U##size, rshift, >>)                     \
binary (size, sub, -)                           \
binary (size, xorb, ^)                          \

all (8)
all (16)
all (32)
all (64)

#undef binary
#undef bothBinary
#undef compare
#undef bothCompare
#undef unary
#undef shift
#undef all
