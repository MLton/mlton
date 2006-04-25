#include "platform.h"

#define Arr(t) Array(t)
#define Vec(t) Vector(t)

#define mkSubSeq(kind, Seq)                                             \
Word##kind##_t PackWord##kind##_sub##Seq (Seq(Word8_t) seq, C_Ptrdiff_t offset) { \
  Word##kind##_t w;                                                     \
  Word8_t* p = (Word8_t*)&w;                                            \
  Word8_t* s = (Word8_t*)seq + offset;                                  \
  int i;                                                                \
                                                                        \
  for (i = 0; i < kind / 8; ++i)                                        \
    p[i] = s[i];                                                        \
  return w;                                                             \
}
#define mkSubSeqRev(kind, Seq)                                          \
Word##kind##_t PackWord##kind##_sub##Seq##Rev (Seq(Word8_t) seq, C_Ptrdiff_t offset) { \
  Word##kind##_t w;                                                     \
  Word8_t* p = (Word8_t*)&w;                                            \
  Word8_t* s = (Word8_t*)seq + offset;                                  \
  int i;                                                                \
                                                                        \
  for (i = 0; i < kind / 8; ++i)                                        \
    p[i] = s[((kind / 8) - 1) - i];                                     \
  return w;                                                             \
}

#define mkUpdate(kind)                                                  \
void PackWord##kind##_update (Arr(Word8_t) a, C_Ptrdiff_t offset, Word##kind##_t w) { \
  Word8_t* p = (Word8_t*)&w;                                            \
  Word8_t* s = (Word8_t*)a + offset;                                    \
  int i;                                                                \
                                                                        \
  for (i = 0; i < kind / 8; ++i)                                        \
    s[i] = p[i];                                                        \
}
#define mkUpdateRev(kind)                                               \
void PackWord##kind##_updateRev (Arr(Word8_t) a, C_Ptrdiff_t offset, Word##kind##_t w) { \
  Word8_t* p = (Word8_t*)&w;                                            \
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

all (8)
all (16)
all (32)
all (64)

#undef mkSubSeq
#undef mkSubSeqRev
#undef mkUpdate
#undef all


Word32_t Word8Array_subWord32Rev (Array(Word8_t) a, C_Ptrdiff_t offset) {
  return PackWord32_subArrRev (a, 4 * offset);
}

void Word8Array_updateWord32Rev (Array(Word32_t) a, C_Ptrdiff_t offset, Word32_t w) {
  PackWord32_updateRev (a, 4 * offset, w);
}

Word32_t Word8Vector_subWord32Rev (Vector(Word8_t) v, C_Ptrdiff_t offset) {
  return PackWord32_subArrRev (v, 4 * offset);
}
