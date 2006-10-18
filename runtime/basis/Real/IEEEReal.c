#include "platform.h"

#if !HAS_FEROUND

#if (defined __i386__)

/* Macros for accessing the hardware control word. */
#define _FPU_GETCW(cw) __asm__ ("fnstcw %0" : "=m" (*&cw))
#define _FPU_SETCW(cw) __asm__ ("fldcw %0" : : "m" (*&cw))

#define ROUNDING_CONTROL_MASK 0x0C00
#define ROUNDING_CONTROL_SHIFT 10

static inline C_Int_t fegetround (void) {
        unsigned short controlWord;

        _FPU_GETCW (controlWord);
        return (controlWord & ROUNDING_CONTROL_MASK) >> ROUNDING_CONTROL_SHIFT;
}

static inline void fesetround (C_Int_t mode) {
        unsigned short controlWord;

        _FPU_GETCW (controlWord);
        controlWord &= ~ROUNDING_CONTROL_MASK;
        controlWord |= mode << ROUNDING_CONTROL_SHIFT;
        _FPU_SETCW (controlWord);
}

#else

#error fe{get,set}round not implemented

#endif

#endif

C_Int_t IEEEReal_getRoundingMode (void) {
  return fegetround ();
}

void IEEEReal_setRoundingMode (C_Int_t m) {
  assert (m != IEEEReal_RoundingMode_FE_NOSUPPORT);
  fesetround (m);
}
