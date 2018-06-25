
#define WordS_addCheckBody(size, x, y, doOverflow, doSuccess)   \
  do {                                                          \
    if (x >= 0) {                                               \
      if (y > WordS##size##_max - x) {                          \
        doOverflow;                                             \
      }                                                         \
    } else if (y < WordS##size##_min - x) {                     \
      doOverflow;                                               \
    }                                                           \
    doSuccess;                                                  \
  } while (0)
#define WordS_addCheckBodyCX(size, c, x, doOverflow, doSuccess) \
WordS_addCheckBody(size, c, x, doOverflow, doSuccess)

#define WordU_addCheckBody(size, x, y, doOverflow, doSuccess)   \
  do {                                                          \
    if (y > Word##size##_max - x) {                             \
      doOverflow;                                               \
    }                                                           \
    doSuccess;                                                  \
  } while (0)
#define WordU_addCheckBodyCX(size, c, x, doOverflow, doSuccess) \
WordU_addCheckBody(size, c, x, doOverflow, doSuccess)

#define WordS_mulCheckBody(size, x, y, doOverflow, doSuccess)   \
  do {                                                          \
    if ((x == (WordS##size)0) || (y == (WordS##size)0)) {       \
    } else                                                      \
    if (x > (WordS## size)0) {                                  \
      if (y > (WordS##size)0) {                                 \
        if (x > WordS##size##_quot (WordS##size##_max, y)) {    \
          doOverflow;                                           \
        }                                                       \
      } else /* (y < (WordS##size)0) */ {                       \
        if (y < WordS##size##_quot (WordS##size##_min, x)) {    \
          doOverflow;                                           \
        }                                                       \
      }                                                         \
    } else /* (x < (WordS##size)0) */ {                         \
      if (y > (WordS##size)0) {                                 \
        if (x < WordS##size##_quot (WordS##size##_min, y)) {    \
          doOverflow;                                           \
        }                                                       \
      } else /* (y < (WordS##size)0) */ {                       \
        if (y < WordS##size##_quot (WordS##size##_max, x)) {    \
          doOverflow;                                           \
        }                                                       \
      }                                                         \
    }                                                           \
    doSuccess;                                                  \
  } while (0)
// #undef WordS_mulCheckBody
#define WordU_mulCheckBody(size, x, y, doOverflow, doSuccess)   \
  do {                                                          \
    if ((x == (WordU##size)0) || (y == (WordU##size)0)) {       \
    } else                                                      \
    if (x > WordU##size##_quot (Word##size##_max, y)) {         \
      doOverflow;                                               \
    }                                                           \
    doSuccess;                                                  \
  } while (0)
// #undef WordU_mulCheckBody
/* #define Word_mulCheckBody(small, large, x, y, doOverflow, doSuccess)    \ */
/*   do {                                                                  \ */
/*     Word##large tmp;                                                    \ */
/*     Word##small res;                                                    \  */
/*     tmp = ((Word##large)x) * ((Word##large)y);                          \ */
/*     res = (Word##small)tmp;                                             \ */
/*     if (tmp != res) {                                                   \ */
/*       doOverflow;                                                       \ */
/*     }                                                                   \ */
/*     doSuccess;                                                          \ */
/*   } while (0) */
/* #define WordS_mulCheckBody(small, large, x, y, doOverflow, doSuccess)   \ */
/* Word_mulCheckBody(S##small, S##large, x, y, doOverflow, doSuccess) */
/* #define WordU_mulCheckBody(small, large, x, y, doOverflow, doSuccess)   \ */
/* Word_mulCheckBody(U##small, U##large, x, y, doOverflow, doSuccess) */

#define Word_negCheckBody(size, x, doOverflow, doSuccess)       \
  do {                                                          \
    if (x == WordS##size##_min) {                               \
      doOverflow;                                               \
    }                                                           \
    doSuccess;                                                  \
  } while (0)

#define WordS_subCheckBodyCX(size, c, x, doOverflow, doSuccess) \
  do {                                                          \
    if (c >= 0) {                                               \
      if (x < c - WordS##size##_max) {                          \
        doOverflow;                                             \
      }                                                         \
    } else if (x > c - WordS##size##_min) {                     \
      doOverflow;                                               \
    }                                                           \
    doSuccess;                                                  \
  } while (0)
#define WordS_subCheckBodyXC(size, x, c, doOverflow, doSuccess) \
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
#define WordS_subCheckBody(size, x, y, doOverflow, doSuccess)   \
WordS_subCheckBodyCX(size, x, y, doOverflow, doSuccess)


#define WordS_addCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_addCheckP (WordS##size x, WordS##size y) {         \
    return __builtin_add_overflow_p (x, y, (WordS##size) 0);            \
  }

#define WordU_addCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordU##size##_addCheckP (WordU##size x, WordU##size y) {         \
    return __builtin_add_overflow_p (x, y, (WordU##size) 0);            \
  }

#define WordS_mulCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_mulCheckP (WordS##size x, WordS##size y) {         \
    return __builtin_mul_overflow_p (x, y, (WordS##size) 0);            \
  }
#define WordU_mulCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordU##size##_mulCheckP (WordU##size x, WordU##size y) {         \
    return __builtin_mul_overflow_p (x, y, (WordU##size) 0);            \
  }

#define Word_negCheckP(size)                                            \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool Word##size##_negCheckP (WordS##size x) {                         \
    return x == WordS##size##_min;                                      \
  }

#define WordS_subCheckP(size)                                           \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_subCheckP (WordS##size x, WordS##size y) {         \
    return __builtin_sub_overflow_p (x, y, (WordS##size) 0);            \
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
