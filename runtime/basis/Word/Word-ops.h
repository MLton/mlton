#define binary(kind, name, op)                                          \
  PRIVATE INLINE                                                        \
  Word##kind Word##kind##_##name (Word##kind w1, Word##kind w2) {       \
    return w1 op w2;                                                    \
  }

#define bothBinary(size, name, op)              \
binary (S##size, name, op)                      \
binary (U##size, name, op)

/* Use `__builtin_<op>_overflow` for `Word<N>_<op>`,
 * because it has defined semantics even if the operation overflows
 * and to encourage fusing with matching `Word<N>_<op>CheckP`.
 */
#define binaryOvflOp(kind, name)                                        \
  PRIVATE INLINE                                                        \
  Word##kind Word##kind##_##name (Word##kind w1, Word##kind w2) {       \
    Word##kind res;                                                     \
    __builtin_##name##_overflow(w1, w2, &res);                          \
    return res;                                                         \
  }

#define bothBinaryOvflOp(size, name)            \
binaryOvflOp (S##size, name)                    \
binaryOvflOp (U##size, name)

#define binaryOvflChk(kind, name)                                       \
  PRIVATE INLINE                                                        \
  Bool Word##kind##_##name##CheckP (Word##kind w1, Word##kind w2) {     \
    Word##kind res;                                                     \
    return __builtin_##name##_overflow(w1, w2, &res);                   \
  }

#define bothBinaryOvflChk(size, name)           \
binaryOvflChk (S##size, name)                   \
binaryOvflChk (U##size, name)

#define binaryOvflOpAndChk(kind, name)                                  \
  PRIVATE INLINE                                                        \
  void Word##kind##_##name##AndCheck (Word##kind w1, Word##kind w2, Word##kind *rw, Bool *rb) {  \
    *rb = __builtin_##name##_overflow(w1, w2, rw);                      \
  }

#define bothBinaryOvflOpAndChk(size, name)      \
binaryOvflOpAndChk (S##size, name)              \
binaryOvflOpAndChk (U##size, name)

#define compare(kind, name, op)                                         \
  PRIVATE INLINE                                                        \
  Bool Word##kind##_##name (Word##kind w1, Word##kind w2) {             \
    return w1 op w2;                                                    \
  }

#define bothCompare(size, name, op)             \
compare (S##size, name, op)                     \
compare (U##size, name, op)

#define negOvflOp(kind)                                                 \
  PRIVATE INLINE                                                        \
  Word##kind Word##kind##_neg (Word##kind w) {                          \
    Word##kind res;                                                     \
    __builtin_sub_overflow(0, w, &res);                                 \
    return res;                                                         \
  }

#define negOvflChk(kind)                                                \
  PRIVATE INLINE                                                        \
  Bool Word##kind##_negCheckP (Word##kind w) {                          \
    Word##kind res;                                                     \
    return __builtin_sub_overflow(0, w, &res);                          \
  }

#define negOvflOpAndChk(kind)                                           \
  PRIVATE INLINE                                                        \
  void Word##kind##_negAndCheck (Word##kind w, Word##kind *rw, Bool *rb) { \
    *rb = __builtin_sub_overflow(0, w, rw);                             \
  }

#define rol(size)                                                       \
  PRIVATE INLINE                                                        \
  Word##size Word##size##_rol (Word##size w1, Word32 w2) {              \
    return (Word##size)(w1 >> (size - w2)) | (Word##size)(w1 << w2);    \
  }

#define ror(size)                                                       \
  PRIVATE INLINE                                                        \
  Word##size Word##size##_ror (Word##size w1, Word32 w2) {              \
    return (Word##size)(w1 >> w2) | (Word##size)(w1 << (size - w2));    \
  }                                                                     \

#define shift(kind, name, op)                                           \
  PRIVATE INLINE                                                        \
  Word##kind Word##kind##_##name (Word##kind w1, Word32 w2) {           \
    return (Word##kind)(w1 op w2);                                      \
  }

#define unary(kind, name, op)                                           \
  PRIVATE INLINE                                                        \
  Word##kind Word##kind##_##name (Word##kind w) {                       \
    return (Word##kind)(op w);                                          \
  }

#define misaligned(size)                                                \
  PRIVATE INLINE                                                        \
  Word##size##_t Word##size##_fetch (Ref(Word##size##_t) wp) {          \
    Word##size##_t w;                                                   \
    memcpy(&w, wp, sizeof(Word##size##_t));                             \
    return w;                                                           \
  }                                                                     \
  PRIVATE INLINE                                                        \
  void Word##size##_store (Ref(Word##size##_t) wp, Word##size##_t w) {  \
    memcpy(wp, &w, sizeof(Word##size##_t));                             \
    return;                                                             \
  }                                                                     \
  PRIVATE INLINE                                                        \
  void Word##size##_move (Ref(Word##size##_t) dst, Ref(Word##size##_t) src) { \
    memcpy(dst, src, sizeof(Word##size##_t));                           \
    return;                                                             \
  }

#define all(size)                               \
binaryOvflOp (size, add)                        \
bothBinaryOvflChk (size, add)                   \
bothBinaryOvflOpAndChk (size, add)              \
binary (size, andb, &)                          \
compare (size, equal, ==)                       \
bothCompare (size, ge, >=)                      \
bothCompare (size, gt, >)                       \
bothCompare (size, le, <=)                      \
shift (size, lshift, <<)                        \
bothCompare (size, lt, <)                       \
bothBinaryOvflOp (size, mul)                    \
bothBinaryOvflChk (size, mul)                   \
bothBinaryOvflOpAndChk (size, mul)              \
negOvflOp (size)                                \
negOvflChk (S##size)                            \
negOvflChk (U##size)                            \
negOvflOpAndChk (S##size)                       \
negOvflOpAndChk (U##size)                       \
unary (size, notb, ~)                           \
bothBinary (size, quot, /)                      \
bothBinary (size, rem, %)                       \
binary (size, orb, |)                           \
rol(size)                                       \
ror(size)                                       \
/* WordS<N>_rshift has implementation-defined behavior under C11.
 * "The result of E1 >> E2 is E1 right-shifted E2 bit positions. If E1 has a
 * signed type and a negative value, the resulting value is
 * implementation-defined."
 * However, gcc and clang implement signed '>>' on negative numbers by sign
 * extension.
 */                                             \
shift (S##size, rshift, >>)                     \
shift (U##size, rshift, >>)                     \
binaryOvflOp (size, sub)                        \
bothBinaryOvflChk (size, sub)                   \
bothBinaryOvflOpAndChk (size, sub)              \
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
#undef negOvflOpAndChk
#undef negOvflChk
#undef negOvflOp
#undef bothCompare
#undef compare
#undef bothBinaryOvflOpAndChk
#undef binaryOvflOpAndChk
#undef bothBinaryOvflChk
#undef binaryOvflChk
#undef bothBinaryOvflOp
#undef binaryOvflOp
#undef bothBinary
#undef binary
