#include "platform.h"

/* ------------------------------------------------- */
/*                     IEEEReal                      */
/* ------------------------------------------------- */

#if (defined (__i386__))

/* Macros for accessing the hardware control word.  */
#define _FPU_GETCW(cw) __asm__ ("fnstcw %0" : "=m" (*&cw))
#define _FPU_SETCW(cw) __asm__ ("fldcw %0" : : "m" (*&cw))

#define ROUNDING_CONTROL_MASK 0x0C00
#define ROUNDING_CONTROL_SHIFT 10

void IEEEReal_setRoundingMode (Int mode) {
	unsigned short controlWord;

	_FPU_GETCW(controlWord);
	controlWord &= ~ROUNDING_CONTROL_MASK;
	controlWord |= mode << ROUNDING_CONTROL_SHIFT;
	_FPU_SETCW(controlWord);
}

Int IEEEReal_getRoundingMode () {
	unsigned short controlWord;

	_FPU_GETCW(controlWord);
	return (controlWord & ROUNDING_CONTROL_MASK) >> ROUNDING_CONTROL_SHIFT;
}

#elif (defined (__ppc__))

#include <ppc_intrinsics.h>

/* PPC rounding control works as follows:
 *
 * The FPSCR (floating point status and control register) contains
 * the rounding mode in bits 30 and 31 (in PPC notation, these are the
 * least significant bits).  These bits have the following meaning:
 *
 * 00   Round to nearest
 * 01   Round toward zero
 * 10   Round toward +Infinity
 * 11   Round toward -Infinity
 *
 * The messed up thing about the way that the FPSCR is accessed is that
 * you can only load from it into a floating point register - so to get
 * at the actual bits, you have to "bounce" the floating point value off
 * memory to place it into an integer register.  A similar problem
 * occurs when you want to place a value into the FPSCR - you have to
 * first get the value into a floating point register first.
 *
 * Source: ARCHPUB1.PDF (you can get it at IBM's developerWorks) */

void IEEEReal_setRoundingMode (Int mode) {
	int ppc_mode;
	
	union {
		double d;
		long long q;
	} u;
	
	switch (mode) {
		/* TO_NEAREST */
		case 0: ppc_mode = 0; break;
		/* TO_NEGINF */
		case 1: ppc_mode = 3; break;
		/* TO_POSINF */
		case 2: ppc_mode = 2; break;
		/* TO_ZERO */
		case 3: ppc_mode = 1; break;
		default:
			die ("IEEEReal_setRoundingMode error: invalid mode %d\n",
				(int)mode);
	}
	u.d=__mffs();
	u.q=(u.q&~3)|ppc_mode;
	__mtfsf(255,u.d);	/* indicate that I wish to set all fields; this is the
					       recommended thing to do according to the IBM manual */
}

Int IEEEReal_getRoundingMode () {
	union {
		double d;
		long long q;
	} u;
	
	u.d=__mffs();
	
	switch (u.q&3) {
		/* TO_NEAREST */
		case 0: return 0;
		/* TO_NEGINF */
		case 3: return 1;
		/* TO_POSINF */
		case 2: return 2;
		/* TO_ZERO */
		case 1: return 3;
		default:
			die ("IEEEReal_getRoundingMode error: apparently, the compiler didn't understand clearly when I said &~3.\n");
	}
}

#elif (defined (__sparc__))

#include <ieeefp.h>

void IEEEReal_setRoundingMode (Int mode) {
	switch (mode) {
	case 0: mode = FP_RN; break;
	case 1: mode = FP_RM; break;
	case 2: mode = FP_RP; break;
	case 3: mode = FP_RZ; break;
	default:
		die ("IEEEReal_setRoundingMode error: invalid mode %d\n", 
			(int)mode);
	}
	fpsetround (mode);
}
 
Int IEEEReal_getRoundingMode () {
	int mode;

	mode = fpgetround ();
	switch (mode) {
	case FP_RN: mode = 0; break;
	case FP_RM: mode = 1; break;
 	case FP_RP: mode = 2; break;
	case FP_RZ: mode = 3; break;
	default:
		die ("IEEEReal_setRoundingMode error: invalid mode %d\n", mode);
	}
	return mode;
}

#else

#error IEEEReal_{get,set}RoundingMode not defined

#endif
