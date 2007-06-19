
#define coerce(n, f, t)                         \
  MLTON_CODEGEN_STATIC_INLINE                   \
  t f##_##n##To##t (f x) {                      \
    return (t)x;                                \
  }
#define bothFromWordCoerce(name, from, to)      \
coerce (name, Word##S##from, to)                \
coerce (name, Word##U##from, to)
#define bothToWordCoerce(name, from, to)        \
coerce (name, from, Word##S##to)                \
coerce (name, from, Word##U##to)

#define allWord(size)                           \
bothFromWordCoerce(rnd, size, Real32)           \
bothFromWordCoerce(rnd, size, Real64)           \
bothToWordCoerce(rnd, Real32, size)             \
bothToWordCoerce(rnd, Real64, size)             \
bothFromWordCoerce(extd, size, Word8)           \
bothFromWordCoerce(extd, size, Word16)          \
bothFromWordCoerce(extd, size, Word32)          \
bothFromWordCoerce(extd, size, Word64)

allWord(8)
allWord(16)
allWord(32)
allWord(64)

#undef all
#undef bothToWordCoerce
#undef bothFromWordCoerce

coerce(rnd, Real32, Real32)
coerce(rnd, Real32, Real64)
coerce(rnd, Real64, Real32)
coerce(rnd, Real64, Real64)

#undef coerce

#define cast(f, t)                              \
  MLTON_CODEGEN_STATIC_INLINE                   \
  t f##_castTo##t (f x) {                       \
    t y;                                        \
    memcpy(&y, &x, sizeof(t));                  \
    return y;                                   \
  }

cast(Real32, Word32)
cast(Word32, Real32)
cast(Real64, Word64)
cast(Word64, Real64)

#undef cast
