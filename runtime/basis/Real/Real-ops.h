
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

/* ternary fma{f} operations
 * These differ based on size and do not rely on a specific operation, so
 * the definitions must be made somewhat explicitly (is no explicit mul_sub)
 * 
 * Defs were designed to conform to the rest of the defs as much as possible
 * (so can be passed into all in the same way)
 */
/* to differentiate between fma functions */
#define FMA_FUNC_SUFFIX(size) FMA_FUNC_SUFFIX_##size
#define FMA_FUNC_SUFFIX_32 f
#define FMA_FUNC_SUFFIX_64

#define ternary__(size, suff)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Real##size##_t Real##size##_muladd (Real##size##_t r1, Real##size##_t r2, Real##size##_t r3) { \
    return fma ##suff (r1, r2, r3);                                     \
  }                                                                     \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Real##size##_t Real##size##_mulsub (Real##size##_t r1, Real##size##_t r2, Real##size##_t r3) { \
    return fma ##suff (r1, r2, -r3);                                    \
  }
#define ternary_(size, suff) ternary__(size, suff)
#define ternary(size) ternary_(size, FMA_FUNC_SUFFIX(size))

#define unary(size, name, op)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Real##size##_t Real##size##_##name (Real##size##_t r1) {              \
    return op r1;                                                       \
  }

#define misaligned(size)                                                \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Real##size##_t Real##size##_fetch (Ref(Real##size##_t) rp) {          \
    Real##size##_t r;                                                   \
    memcpy(&r, rp, sizeof(Real##size##_t));                             \
    return r;                                                           \
  }                                                                     \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  void Real##size##_store (Ref(Real##size##_t) rp, Real##size##_t r) {  \
    memcpy(rp, &r, sizeof(Real##size##_t));                             \
    return;                                                             \
  }                                                                     \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  void Real##size##_move (Ref(Real##size##_t) dst, Ref(Real##size##_t) src) { \
    memcpy(dst, src, sizeof(Real##size##_t));                           \
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
ternary(size)                                   \
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
