
#define binary(size, name, op)                                          \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Real##size##_t Real##size##_##name (Real##size##_t r1, Real##size##_t r2) { \
    return r1 op r2;                                                    \
  }

#define compare(size, name, op)                                         \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool Real##size##_##name (Real##size##_t r1, Real##size##_t r2) {     \
    return r1 op r2;                                                    \
  }

#define ternary(size, name, op)                                         \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Real##size##_t Real##size##_mul##name (Real##size##_t r1, Real##size##_t r2, Real##size##_t r3) { \
    return r1 * r2 op r3;                                               \
  }

#define unary(size, name, op)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Real##size##_t Real##size##_##name (Real##size##_t r1) {              \
    return op r1;                                                       \
  }

#define misaligned(size)                                                \
  typedef volatile union {                                              \
    Real##size##_t r;                                                   \
    Word32_t ws[sizeof(Real##size##_t) / sizeof(Word32_t)];             \
  } Real##size##OrWord32s;                                              \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Real##size##_t Real##size##_fetch (Ref(Real##size##_t) rp) {          \
    Real##size##OrWord32s u;                                            \
    Word32_t *wsp;                                                      \
    wsp = (Word32_t*)rp;                                                \
    u.ws[0] = wsp[0];                                                   \
    if ((sizeof(Real##size##_t) / sizeof(Word32_t)) > 1)                \
      u.ws[1] = wsp[1];                                                 \
    return u.r;                                                         \
  }                                                                     \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  void Real##size##_store (Ref(Real##size##_t) rp, Real##size##_t r) {  \
    Real##size##OrWord32s u;                                            \
    Word32_t *wsp;                                                      \
    wsp = (Word32_t*)rp;                                                \
    u.r = r;                                                            \
    wsp[0] = u.ws[0];                                                   \
    if ((sizeof(Real##size##_t) / sizeof(Word32_t)) > 1)                \
      wsp[1] = u.ws[1];                                                 \
    return;                                                             \
  }                                                                     \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  void Real##size##_move (Ref(Real##size##_t) dst, Ref(Real##size##_t) src) { \
    Real##size##_t r;                                                   \
    r = Real##size##_fetch (src);                                       \
    Real##size##_store (dst, r);                                        \
    return;                                                             \
  }

#define all(size)                               \
binary(size, add, +)                            \
binary(size, div, /)                            \
binary(size, mul, *)                            \
binary(size, sub, -)                            \
compare(size, equal, ==)                        \
compare(size, le, <=)                           \
compare(size, lt, <)                            \
ternary(size, add, +)                           \
ternary(size, sub, -)                           \
unary(size, neg, -)                             \
misaligned(size)

all(32)
all(64)

#undef all
#undef misaligned
#undef unary
#undef ternary
#undef compare
#undef binary
