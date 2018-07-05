#if __GNUC__ < 7
#define add_overflow_b(x, y, size, onOverflow, onSuccess)       \
  do {                                                          \
    WordS##size res;                                            \
    if (__builtin_add_overflow(x, y, &res)) {                   \
      onOverflow;                                               \
    }                                                           \
    onSuccess;                                                  \
  } while (0)
#define sub_overflow_b(x, y, size, onOverflow, onSuccess)       \
  do {                                                          \
    WordS##size res;                                            \
    if (__builtin_sub_overflow(x, y, &res)) {                   \
      onOverflow;                                               \
    }                                                           \
    onSuccess;                                                  \
  } while (0)
#define mul_overflow_b(x, y, size, onOverflow, onSuccess)       \
  do {                                                          \
    WordS##size res;                                            \
    if (__builtin_mul_overflow(x, y, &res)) {                   \
      onOverflow;                                               \
    }                                                           \
    onSuccess;                                                  \
  } while (0)
#else
#define add_overflow_b(x, y, size, onOverflow, onSuccess)       \
  do {                                                          \
    if (__builtin_add_overflow_p(x, y, (WordS##size) 0)) {      \
      onOverflow;                                               \
    }                                                           \
    onSuccess;                                                  \
  } while (0)
#define sub_overflow_b(x, y, size, onOverflow, onSuccess)       \
  do {                                                          \
    if (__builtin_sub_overflow_p(x, y, (WordS##size) 0)) {      \
      onOverflow;                                               \
    }                                                           \
    onSuccess;                                                  \
  } while (0)
#define mul_overflow_b(x, y, size, onOverflow, onSuccess)       \
  do {                                                          \
    if (__builtin_mul_overflow_p(x, y, (WordS##size) 0)) {      \
      onOverflow;                                               \
    }                                                           \
    onSuccess;                                                  \
  } while (0)
#endif
#define neg_overflow_b(x, size, onOverflow, onSuccess)          \
  sub_overflow_b(0, x, size, onOverflow, onSuccess)

/**
 * Old-style overflow operators
 */

#define WordS_addCheckBody(size, x, y, doOverflow, doSuccess)   \
  add_overflow_b(x, y, size, doOverflow, doSuccess)
#define WordS_addCheckBodyCX(size, c, x, doOverflow, doSuccess) \
  WordS_addCheckBody(size, c, x, doOverflow, doSuccess)

#define WordU_addCheckBody(size, x, y, doOverflow, doSuccess)   \
  add_overflow_b(x, y, size, doOverflow, doSuccess)
#define WordU_addCheckBodyCX(size, c, x, doOverflow, doSuccess) \
  WordU_addCheckBody(size, c, x, doOverflow, doSuccess)

#define WordS_mulCheckBody(size, x, y, doOverflow, doSuccess)   \
  mul_overflow_b(x, y, size, doOverflow, doSuccess)

#define WordU_mulCheckBody(size, x, y, doOverflow, doSuccess)   \
  mul_overflow_b(x, y, size, doOverflow, doSuccess)

#define WordS_negCheckBody(size, x, doOverflow, doSuccess)      \
  neg_overflow_b(x, size, doOverflow, doSuccess)

#define WordS_subCheckBody(size, x, y, doOverflow, doSuccess)   \
  sub_overflow_b(x, y, size, doOverflow, doSuccess)
#define WordS_subCheckBodyCX(size, c, x, doOverflow, doSuccess) \
  WordS_subCheckBody(size, c, x, doOverflow, doSuccess)

#define WordS_subCheckBodyXC(size, x, c, doOverflow, doSuccess) \
  WordS_subCheckBody(size, x, c, doOverflow, doSuccess)

/*
 * Old WordS_subCheckBodyXC:
 *
  do {                                                          \
    if (c <= 0) {                                               \
      if (x > WordS##size##_max + c) {                          \
        doOverflow;                                             \
      }                                                         \
    } else if (x < WordS##size##_min + c) {                     \
      doOverflow;                                               \
    }                                                           \
    doSuccess;                                                  \
  } while (0)
  */

/**
 * New-style overflow operators
 */

#define WordS_addCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_addCheckP (WordS##size x, WordS##size y) {         \
    add_overflow_b(x, y, size, return TRUE, return FALSE);              \
  }

#define WordU_addCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordU##size##_addCheckP (WordU##size x, WordU##size y) {         \
    add_overflow_b(x, y, size, return TRUE, return FALSE);              \
  }

#define WordS_mulCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_mulCheckP (WordS##size x, WordS##size y) {         \
    mul_overflow_b(x, y, size, return TRUE, return FALSE);              \
  }
#define WordU_mulCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordU##size##_mulCheckP (WordU##size x, WordU##size y) {         \
    mul_overflow_b(x, y, size, return TRUE, return FALSE);              \
  }

#define Word_negCheckP(size)                                            \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool Word##size##_negCheckP (WordS##size x) {                         \
    neg_overflow_b(x, size, return TRUE, return FALSE);                 \
  }

#define WordS_subCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_subCheckP (WordS##size x, WordS##size y) {         \
    sub_overflow_b(x, y, size, return TRUE, return FALSE);              \
  }

#define all(size)                               \
WordS_addCheckP(size)                           \
WordU_addCheckP(size)                           \
WordS_mulCheckP(size)                           \
WordU_mulCheckP(size)                           \
Word_negCheckP(size)                            \
WordS_subCheckP(size)

all(8)
all(16)
all(32)
all(64)

#undef all
#undef WordS_subCheckP
#undef WordS_negCheckP
#undef WordU_mulCheckP
#undef WordS_mulCheckP
#undef WordU_addCheckP
#undef WordS_addCheckP
