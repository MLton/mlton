#define FNSUF32 f
#define FNSUF64

#define naryNameFnSufResArgsCall_(size, name, f, suf, rty, args, call)  \
  PRIVATE INLINE                                                        \
  rty Real##size##_##name args {                                        \
    return f##suf call;                                                 \
  }
#define naryNameFnSufResArgsCall(size, name, f, suf, rty, args, call)   \
naryNameFnSufResArgsCall_(size, name, f, suf, rty, args, call)
#define naryNameFnResArgsCall(size, name, f, rty, args, call)           \
naryNameFnSufResArgsCall(size, name, f, FNSUF##size, rty, args, call)

#define binaryOp(size, name, op)                                        \
  PRIVATE INLINE                                                        \
  Real##size##_t Real##size##_##name (Real##size##_t r1, Real##size##_t r2) { \
    return r1 op r2;                                                    \
  }

#define binaryNameFn(size, name, f)                                     \
naryNameFnResArgsCall(size, name, f, Real##size##_t, (Real##size##_t r1, Real##size##_t r2), (r1, r2))

#define binaryFn(size, f)                                               \
binaryNameFn(size, f, f)

#define binaryMathFn(size, f)                                           \
binaryNameFn(size, Math_##f, f)

#define compareNameFn(size, name, f)                                    \
  PRIVATE INLINE                                                        \
  Bool Real##size##_##name (Real##size##_t r1, Real##size##_t r2) {     \
    return f (r1, r2);                                                  \
  }

#define equal(size)                                                     \
  PRIVATE INLINE                                                        \
  Bool Real##size##_equal (Real##size##_t r1, Real##size##_t r2) {      \
    return r1 == r2;                                                    \
  }

#define fmaNameOp(size, name, op)                                       \
naryNameFnResArgsCall(size, name, fma, Real##size##_t, (Real##size##_t r1, Real##size##_t r2, Real##size##_t r3), (r1, r2, op r3))

#define qequal(size)                                                    \
  PRIVATE INLINE                                                        \
  Bool Real##size##_qequal (Real##size##_t r1, Real##size##_t r2) {     \
    return isunordered (r1, r2) || r1 == r2;                            \
  }

#define unaryOp(size, name, op)                                         \
  PRIVATE INLINE                                                        \
  Real##size##_t Real##size##_##name (Real##size##_t r) {               \
    return op r;                                                        \
  }

#define unaryNameFn(size, name, f)                                      \
naryNameFnResArgsCall(size, name, f, Real##size##_t, (Real##size##_t r), (r))

#define unaryFn(size, f)                                                \
unaryNameFn(size, f, f)

#define unaryMathFn(size, f)                                            \
unaryNameFn(size, Math_##f, f)

#define misaligned(size)                                                \
  PRIVATE INLINE                                                        \
  Real##size##_t Real##size##_fetch (Ref(Real##size##_t) rp) {          \
    Real##size##_t r;                                                   \
    memcpy(&r, rp, sizeof(Real##size##_t));                             \
    return r;                                                           \
  }                                                                     \
  PRIVATE INLINE                                                        \
  void Real##size##_store (Ref(Real##size##_t) rp, Real##size##_t r) {  \
    memcpy(rp, &r, sizeof(Real##size##_t));                             \
    return;                                                             \
  }                                                                     \
  PRIVATE INLINE                                                        \
  void Real##size##_move (Ref(Real##size##_t) dst, Ref(Real##size##_t) src) { \
    memcpy(dst, src, sizeof(Real##size##_t));                           \
    return;                                                             \
  }

#define all(size)                               \
unaryNameFn(size, abs, fabs)                    \
binaryOp(size, add, +)                          \
binaryOp(size, div, /)                          \
equal(size)                                     \
naryNameFnResArgsCall(size, frexp, frexp, Real##size##_t, (Real##size##_t r, Ref(C_Int_t) ip), (r, (int*)ip)) \
naryNameFnResArgsCall(size, ldexp, ldexp, Real##size##_t, (Real##size##_t r, C_Int_t i), (r, i)) \
compareNameFn(size, le, islessequal)            \
compareNameFn(size, lt, isless)                 \
naryNameFnResArgsCall(size, modf, modf, Real##size##_t, (Real##size##_t x, Ref(Real##size##_t) yp), (x, (Real##size##_t*)yp)) \
binaryOp(size, mul, *)                          \
fmaNameOp(size, muladd,  )                      \
fmaNameOp(size, mulsub, -)                      \
unaryOp(size, neg, -)                           \
qequal(size)                                    \
unaryNameFn(size, realCeil, ceil)               \
unaryNameFn(size, realFloor, floor)             \
unaryNameFn(size, realTrunc, trunc)             \
unaryNameFn(size, round, rint)                  \
binaryOp(size, sub, -)                          \
unaryMathFn(size, acos)                         \
unaryMathFn(size, asin)                         \
unaryMathFn(size, atan)                         \
binaryMathFn(size, atan2)                       \
unaryMathFn(size, cos)                          \
unaryMathFn(size, cosh)                         \
unaryMathFn(size, exp)                          \
unaryNameFn(size, Math_ln, log)                 \
unaryMathFn(size, log10)                        \
binaryMathFn(size, pow)                         \
unaryMathFn(size, sin)                          \
unaryMathFn(size, sinh)                         \
unaryMathFn(size, sqrt)                         \
unaryMathFn(size, tan)                          \
unaryMathFn(size, tanh)                         \
misaligned(size)

all(32)
all(64)

#undef all
#undef misaligned
#undef unaryMathFn
#undef unaryFn
#undef unaryNameFn
#undef unaryOp
#undef qequal
#undef fmaNameOp
#undef equal
#undef compareNameFn
#undef binaryMathFn
#undef binaryFn
#undef binaryNameFn
#undef binaryOp
#undef naryNameFnResArgsCall
#undef naryNameFnSufResArgsCall
#undef naryNameFnSufResArgsCall_
#undef FNSUF64
#undef FNSUF32
