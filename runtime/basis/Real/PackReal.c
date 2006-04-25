#include "platform.h"

#define Arr(t) Array(t)
#define Vec(t) Vector(t)

#define mkSubSeq(kind, Seq)                                             \
Real##kind##_t PackReal##kind##_sub##Seq (Seq(Word8_t) seq, C_Ptrdiff_t offset) { \
  Real##kind##_t r;                                                     \
  Word8_t* p = (Word8_t*)&r;                                            \
  Word8_t* s = (Word8_t*)seq + offset;                                  \
  int i;                                                                \
                                                                        \
  for (i = 0; i < kind / 8; ++i)                                        \
    p[i] = s[i];                                                        \
  return r;                                                             \
}
#define mkSubSeqRev(kind, Seq)                                          \
Real##kind##_t PackReal##kind##_sub##Seq##Rev (Seq(Word8_t) seq, C_Ptrdiff_t offset) { \
  Real##kind##_t r;                                                     \
  Word8_t* p = (Word8_t*)&r;                                            \
  Word8_t* s = (Word8_t*)seq + offset;                                  \
  int i;                                                                \
                                                                        \
  for (i = 0; i < kind / 8; ++i)                                        \
    p[i] = s[((kind / 8) - 1) - i];                                     \
  return r;                                                             \
}

#define mkUpdate(kind)                                                  \
void PackReal##kind##_update (Arr(Word8_t) a, C_Ptrdiff_t offset, Real##kind##_t r) { \
  Word8_t* p = (Word8_t*)&r;                                            \
  Word8_t* s = (Word8_t*)a + offset;                                    \
  int i;                                                                \
                                                                        \
  for (i = 0; i < kind / 8; ++i)                                        \
    s[i] = p[i];                                                        \
}
#define mkUpdateRev(kind)                                               \
void PackReal##kind##_updateRev (Arr(Word8_t) a, C_Ptrdiff_t offset, Real##kind##_t r) { \
  Word8_t* p = (Word8_t*)&r;                                            \
  Word8_t* s = (Word8_t*)a + offset;                                    \
  int i;                                                                \
                                                                        \
  for (i = 0; i < kind / 8; ++i)                                        \
    s[i] = p[((kind / 8) - 1) - i];                                     \
}

#define all(size)                                                       \
        mkSubSeq(size, Arr)                                             \
        mkSubSeq(size, Vec)                                             \
        mkSubSeqRev(size, Arr)                                          \
        mkSubSeqRev(size, Vec)                                          \
        mkUpdate(size)                                                  \
        mkUpdateRev(size)

all (32)
all (64)

#undef mkSubSeq
#undef mkSubSeqRev
#undef mkUpdate
#undef all
