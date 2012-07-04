
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
    return (Word##size)(w1 >> (size - w2)) | (Word##size)(w1 << w2);    \
  }

#define ror(size)                                                       \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Word##size Word##size##_ror (Word##size w1, Word32 w2) {              \
    return (Word##size)(w1 >> w2) | (Word##size)(w1 << (size - w2));    \
  }                                                                     \

#define shift(kind, name, op)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Word##kind Word##kind##_##name (Word##kind w1, Word32 w2) {           \
    return (Word##kind)(w1 op w2);                                      \
  }

#define unary(kind, name, op)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Word##kind Word##kind##_##name (Word##kind w) {                       \
    return (Word##kind)(op w);                                          \
  }

#define misaligned(size)                                                \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Word##size##_t Word##size##_fetch (Ref(Word##size##_t) wp) {          \
    Word##size##_t w;                                                   \
    memcpy(&w, wp, sizeof(Word##size##_t));                             \
    return w;                                                           \
  }                                                                     \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  void Word##size##_store (Ref(Word##size##_t) wp, Word##size##_t w) {  \
    memcpy(wp, &w, sizeof(Word##size##_t));                             \
    return;                                                             \
  }                                                                     \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  void Word##size##_move (Ref(Word##size##_t) dst, Ref(Word##size##_t) src) { \
    memcpy(dst, src, sizeof(Word##size##_t));                           \
    return;                                                             \
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
 */                                                                     \
MLTON_CODEGEN_WORDSQUOTREM_IMPL(binary (S##size, quot, /))              \
MLTON_CODEGEN_WORDSQUOTREM_IMPL(binary (S##size, rem, %))               \
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
binary (size, xorb, ^)

all (8)
all (16)
all (32)
all (64)

misaligned(64)

#undef all
#undef misaligned
#undef unary
#undef shift
#undef ror
#undef rol
#undef bothCompare
#undef compare
#undef bothBinary
#undef binary
