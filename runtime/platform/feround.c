#if (defined(__UCLIBC__))

/* Use whatever we got from fpu_control.h for this CPU model */
#define FE_MASK (FE_DOWNWARD|FE_TONEAREST|FE_TOWARDZERO|FE_UPWARD)

int fegetround () {
        fpu_control_t controlWord;
        _FPU_GETCW(controlWord);
        return controlWord & FE_MASK;
}

int fesetround (int mode) {
        fpu_control_t controlWord;

        _FPU_GETCW (controlWord);
        controlWord = (controlWord & ~FE_MASK) | mode;
        _FPU_SETCW (controlWord);
        return 0;
}

#elif (defined __i386__)

/* Macros for accessing the hardware control word. */
#define _FPU_GETCW(cw) __asm__ ("fnstcw %0" : "=m" (*&cw))
#define _FPU_SETCW(cw) __asm__ ("fldcw %0" : : "m" (*&cw))

/* This assumes the FE_* macros are 0-3 as set by platform.h defaults */
#define ROUNDING_CONTROL_MASK 0x0C00
#define ROUNDING_CONTROL_SHIFT 10

int fegetround () {
        unsigned short controlWord;

        _FPU_GETCW (controlWord);
        return (controlWord & ROUNDING_CONTROL_MASK) >> ROUNDING_CONTROL_SHIFT;
}

int fesetround (int mode) {
        unsigned short controlWord;

        _FPU_GETCW (controlWord);
        controlWord &= ~ROUNDING_CONTROL_MASK;
        controlWord |= mode << ROUNDING_CONTROL_SHIFT;
        _FPU_SETCW (controlWord);
        return 0;
}

#else

#error fe{get,set}round not implemented

#endif
