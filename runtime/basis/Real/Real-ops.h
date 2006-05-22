
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
unary(size, neg, -)

all(32)
all(64)

#undef all
#undef unary
#undef ternary
#undef compare
#undef binary
