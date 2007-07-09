
#define unaryReal(g, h)                                         \
  MLTON_CODEGEN_MATHFN(Real64_t h(Real64_t x);)                 \
  MLTON_CODEGEN_STATIC_INLINE                                   \
  Real64_t Real64_##g (Real64_t x) {                            \
    return h (x);                                               \
  }                                                             \
  MLTON_CODEGEN_MATHFN(Real32_t h##f(Real32_t x);)              \
  MLTON_CODEGEN_STATIC_INLINE                                   \
  Real32_t Real32_##g (Real32_t x) {                            \
    return h##f (x);                                            \
  }
unaryReal(abs, fabs)
unaryReal(round, rint)
#undef unaryReal

#define binaryReal(g, h)                                        \
  MLTON_CODEGEN_MATHFN(Real64_t h(Real64_t x, Real64_t y);)     \
  MLTON_CODEGEN_STATIC_INLINE                                   \
  Real64_t Real64_Math_##g (Real64_t x, Real64_t y) {           \
    return h (x, y);                                            \
  }                                                             \
  MLTON_CODEGEN_MATHFN(Real32_t h##f(Real32_t x, Real32_t y);)  \
  MLTON_CODEGEN_STATIC_INLINE                                   \
  Real32_t Real32_Math_##g (Real32_t x, Real32_t y) {           \
    return h##f (x, y);                                         \
  }
binaryReal(atan2, atan2)
binaryReal(pow, pow)
#undef binaryReal

#define unaryReal(g, h)                                         \
  MLTON_CODEGEN_MATHFN(Real64_t h(Real64_t x);)                 \
  MLTON_CODEGEN_STATIC_INLINE                                   \
  Real64_t Real64_Math_##g (Real64_t x) {                       \
    return h (x);                                               \
  }                                                             \
  MLTON_CODEGEN_MATHFN(Real32_t h##f(Real32_t x);)              \
  MLTON_CODEGEN_STATIC_INLINE                                   \
  Real32_t Real32_Math_##g (Real32_t x) {                       \
    return h##f (x);                                            \
  }
unaryReal(acos, acos)
unaryReal(asin, asin)
unaryReal(atan, atan)
unaryReal(cos, cos)
unaryReal(cosh, cosh)
unaryReal(exp, exp)
unaryReal(ln, log)
unaryReal(log10, log10)
unaryReal(sin, sin)
unaryReal(sinh, sinh)
unaryReal(sqrt, sqrt)
unaryReal(tan, tan)
unaryReal(tanh, tanh)
#undef unaryReal

#define binaryRealIntRef(g, h)                                  \
  MLTON_CODEGEN_MATHFN(Real64_t h (Real64_t x, int* ip);)       \
  MLTON_CODEGEN_STATIC_INLINE                                   \
  Real64_t Real64_##g (Real64_t x, Ref(C_Int_t) i) {            \
    return h (x, (int*)i);                                      \
  }                                                             \
  MLTON_CODEGEN_MATHFN(Real32_t h##f (Real32_t x, int* ip);)    \
  MLTON_CODEGEN_STATIC_INLINE                                   \
  Real32_t Real32_##g (Real32_t x, Ref(C_Int_t) i) {            \
    return h##f (x, (int*)i);                                   \
  }
binaryRealIntRef(frexp, frexp)
#undef binaryRealIntRef

#define binaryRealInt(g, h)                                     \
  MLTON_CODEGEN_MATHFN(Real64_t h (Real64_t x, int i);)         \
  MLTON_CODEGEN_STATIC_INLINE                                   \
  Real64_t Real64_##g (Real64_t x, C_Int_t i) {                 \
    return h (x, i);                                            \
  }                                                             \
  MLTON_CODEGEN_MATHFN(Real32_t h##f (Real32_t x, int i);)      \
  MLTON_CODEGEN_STATIC_INLINE                                   \
  Real32_t Real32_##g (Real32_t x, C_Int_t i) {                 \
    return h##f (x, i);                                         \
  }
binaryRealInt(ldexp, ldexp)
#undef binaryRealInt

#if (defined (__hppa__) || defined (__sparc__))
#define binaryRealRealRef(g, h)                                         \
  MLTON_CODEGEN_MATHFN(Real32_t h##f (Real32_t x, Real32_t *yp);)       \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Real32_t Real32_##g (Real32_t x, Ref(Real32_t) yp) {                  \
    /* Real32_t r, res; */                                              \
    /* r = Real32_fetch (yp); */                                        \
    /* res = h##f (x, &r); */                                           \
    /* Real32_store (yp, r); */                                         \
    /* return res; */                                                   \
    return h##f (x, (Real32_t*)yp);                                     \
  }                                                                     \
  MLTON_CODEGEN_MATHFN(Real64_t h (Real64_t x, Real64_t *yp);)          \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Real64_t Real64_##g (Real64_t x, Ref(Real64_t) yp) {                  \
    Real64_t r, res;                                                    \
    /* r = Real64_fetch (yp); */                                        \
    res = h (x, &r);                                                    \
    Real64_store (yp, r);                                               \
    return res;                                                         \
  }
#else
#define binaryRealRealRef(g, h)                                         \
  MLTON_CODEGEN_MATHFN(Real32_t h##f (Real32_t x, Real32_t *yp);)       \
    MLTON_CODEGEN_STATIC_INLINE                                         \
  Real32_t Real32_##g (Real32_t x, Ref(Real32_t) yp) {                  \
    return h##f (x, (Real32_t*)yp);                                     \
  }                                                                     \
  MLTON_CODEGEN_MATHFN(Real64_t h (Real64_t x, Real64_t *yp);)          \
    MLTON_CODEGEN_STATIC_INLINE                                         \
  Real64_t Real64_##g (Real64_t x, Ref(Real64_t) yp) {                  \
    return h (x, (Real64_t*)yp);                                        \
  }
#endif
binaryRealRealRef(modf, modf)
#undef binaryRealRealRef
