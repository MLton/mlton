
#define coerce(f, t)                            \
  MLTON_CODEGEN_STATIC_INLINE                   \
  t f##_to##t (f x) {                           \
    return (t)x;                                \
  }
#define bothFromWordCoerce(from, to)            \
coerce (Word##S##from, to)                      \
coerce (Word##U##from, to)
#define bothToWordCoerce(from, to)              \
coerce (from, Word##S##to)                      \
coerce (from, Word##U##to)

#define allWord(size)                           \
bothFromWordCoerce(size, Real32)                \
bothFromWordCoerce(size, Real64)                \
bothToWordCoerce(Real32, size)                  \
bothToWordCoerce(Real64, size)                  \
bothFromWordCoerce(size, Word8)                 \
bothFromWordCoerce(size, Word16)                \
bothFromWordCoerce(size, Word32)                \
bothFromWordCoerce(size, Word64)

allWord(8)
allWord(16)
allWord(32)
allWord(64)

#undef all
#undef bothToWordCoerce
#undef bothFromWordCoerce

coerce(Real32,Real32)
coerce(Real32,Real64)
coerce(Real64,Real32)
coerce(Real64,Real64)

#undef coerce
