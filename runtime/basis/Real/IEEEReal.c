#include "platform.h"

#if !HAS_FEROUND

#if (defined __i386__) || (defined __x86_64__)

/* Macros for accessing the hardware control word. */
#define _FPU_GETCW(cw)   __asm__ ("fnstcw %0" : "=m" (*&cw))
#define _FPU_SETCW(cw)   __asm__ ("fldcw %0" : : "m" (*&cw))
#define _SSE_GETCSR(csr) __asm__ ("stmxcsr %0" : "=m" (*&csr))
#define _SSE_SETCSR(csr) __asm__ ("ldmxcsr %0" : : "m" (*&csr))

#define FPU_ROUNDING_CONTROL_MASK 0x0C00
#define FPU_ROUNDING_CONTROL_SHIFT 10
#define SSE_ROUNDING_CONTROL_MASK 0x00006000
#define SSE_ROUNDING_CONTROL_SHIFT 13

static inline C_Int_t fegetround (void) {
        uint16_t fpuControl;
        
        _FPU_GETCW (fpuControl);
        return (fpuControl & FPU_ROUNDING_CONTROL_MASK) 
               >> FPU_ROUNDING_CONTROL_SHIFT;
}

static inline void fesetround (C_Int_t mode) {
        uint16_t fpuControl;
#ifdef __x86_64__
        uint32_t sseControl;
#endif

        _FPU_GETCW (fpuControl);
        fpuControl &= ~FPU_ROUNDING_CONTROL_MASK;
        fpuControl |= mode << FPU_ROUNDING_CONTROL_SHIFT;
        _FPU_SETCW (fpuControl);

#ifdef __x86_64__
        _SSE_GETCSR (sseControl);
        sseControl &= ~SSE_ROUNDING_CONTROL_MASK;
        sseControl |= mode << SSE_ROUNDING_CONTROL_SHIFT;
        _SSE_SETCSR (sseControl);
#endif
}

#elif (defined __UCLIBC__)

/* Use whatever we got from fpu_control.h for this CPU model */
#define FE_MASK (FE_DOWNWARD|FE_TONEAREST|FE_TOWARDZERO|FE_UPWARD)

static inline int fegetround () {
        fpu_control_t controlWord;
        _FPU_GETCW(controlWord);
        return controlWord & FE_MASK;
}

static inline int fesetround (int mode) {
        fpu_control_t controlWord;

        _FPU_GETCW (controlWord);
        controlWord = (controlWord & ~FE_MASK) | mode;
        _FPU_SETCW (controlWord);
        return 0;
}


#else

#error fe{get,set}round not implemented

#endif

#endif /* !HAS_FEROUND */

C_Int_t IEEEReal_getRoundingMode (void) {
  return fegetround ();
}

void IEEEReal_setRoundingMode (C_Int_t m) {
  assert (m != IEEEReal_RoundingMode_FE_NOSUPPORT);
  fesetround (m);
}
