
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

#define WordS_negCheckBody(size, x, doOverflow, doSuccess)      \
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


#define WordS_addCheckOverflows(size)                                   \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_addCheckOverflows (WordS##size x, WordS##size y) { \
    WordS_addCheckBody(size, x, y, return TRUE, return FALSE);          \
  }

#define WordU_addCheckOverflows(size)                                   \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordU##size##_addCheckOverflows (WordU##size x, WordU##size y) { \
    WordU_addCheckBody(size, x, y, return TRUE, return FALSE);          \
  }

#define WordS_mulCheckOverflows(size)                                   \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_mulCheckOverflows (WordS##size x, WordS##size y) { \
    WordS_mulCheckBody(size, x, y, return TRUE, return FALSE);          \
  }
#define WordU_mulCheckOverflows(size)                                   \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordU##size##_mulCheckOverflows (WordU##size x, WordU##size y) { \
    WordU_mulCheckBody(size, x, y, return TRUE, return FALSE);          \
  }

#define WordS_negCheckOverflows(size)                                   \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_negCheckOverflows (WordS##size x) {                \
    WordS_negCheckBody(size, x, return TRUE, return FALSE);             \
  }

#define WordS_subCheckOverflows(size)                                   \
  MLTON_CODEGEN_STATIC_INLINE                                           \
  Bool WordS##size##_subCheckOverflows (WordS##size x, WordS##size y) { \
    WordS_subCheckBody(size, x, y, return TRUE, return FALSE);          \
  }

#define all(size)                               \
WordS_addCheckOverflows(size)                   \
WordU_addCheckOverflows(size)                   \
WordS_mulCheckOverflows(size)                   \
WordU_mulCheckOverflows(size)                   \
WordS_negCheckOverflows(size)                   \
WordS_subCheckOverflows(size)

all(8)
all(16)
all(32)
all(64)

#undef all
#undef WordS_subCheckOverflows
#undef WordS_negCheckOverflows
#undef WordU_mulCheckOverflows
#undef WordS_mulCheckOverflows
#undef WordU_addCheckOverflows
#undef WordS_addCheckOverflows
