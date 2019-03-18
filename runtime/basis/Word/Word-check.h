#define add_overflow_b(x, y, kind, onOverflow, onSuccess)       \
  do {                                                          \
    Word##kind res;                                             \
    if (__builtin_add_overflow(x, y, &res)) {                   \
      onOverflow;                                               \
    }                                                           \
    onSuccess;                                                  \
  } while (0)
#define sub_overflow_b(x, y, kind, onOverflow, onSuccess)       \
  do {                                                          \
    Word##kind res;                                             \
    if (__builtin_sub_overflow(x, y, &res)) {                   \
      onOverflow;                                               \
    }                                                           \
    onSuccess;                                                  \
  } while (0)
#define mul_overflow_b(x, y, kind, onOverflow, onSuccess)       \
  do {                                                          \
    Word##kind res;                                             \
    if (__builtin_mul_overflow(x, y, &res)) {                   \
      onOverflow;                                               \
    }                                                           \
    onSuccess;                                                  \
  } while (0)
#define neg_overflow_b(x, kind, onOverflow, onSuccess)          \
  do {                                                          \
    Word##kind res;                                             \
    if (__builtin_sub_overflow(0, x, &res)) {                   \
      onOverflow;                                               \
    }                                                           \
    onSuccess;                                                  \
  } while (0)

#define WordS_addCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_addCheckP (WordS##size x, WordS##size y) {         \
    add_overflow_b(x, y, S##size, return TRUE, return FALSE);           \
  }

#define WordU_addCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordU##size##_addCheckP (WordU##size x, WordU##size y) {         \
    add_overflow_b(x, y, U##size, return TRUE, return FALSE);           \
  }

#define WordS_mulCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_mulCheckP (WordS##size x, WordS##size y) {         \
    mul_overflow_b(x, y, S##size, return TRUE, return FALSE);           \
  }
#define WordU_mulCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordU##size##_mulCheckP (WordU##size x, WordU##size y) {         \
    mul_overflow_b(x, y, U##size, return TRUE, return FALSE);           \
  }

#define Word_negCheckP(size)                                            \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool Word##size##_negCheckP (WordS##size x) {                         \
    neg_overflow_b(x, S##size, return TRUE, return FALSE);              \
  }

#define WordS_subCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_subCheckP (WordS##size x, WordS##size y) {         \
    sub_overflow_b(x, y, S##size, return TRUE, return FALSE);           \
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
#undef Word_negCheckP
#undef WordU_mulCheckP
#undef WordS_mulCheckP
#undef WordU_addCheckP
#undef WordS_addCheckP
