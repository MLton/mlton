#include "platform.h"

Real32 Real32_Math_pi = (Real32)3.14159265358979323846;
Real32 Real32_Math_e = (Real32)2.7182818284590452354;

Real32 Real32_maxFinite =    3.40282347e+38;
Real32 Real32_minNormalPos = 1.17549435e-38;
Real32 Real32_minPos =       1.40129846e-45;

Real64 Real64_Math_pi = 3.14159265358979323846;
Real64 Real64_Math_e = 2.7182818284590452354;

Real64 Real64_maxFinite =    1.7976931348623157e+308;
Real64 Real64_minNormalPos = 2.2250738585072014e-308;
Real64 Real64_minPos =       4.9406564584124654e-324;

#define ternary(size, name, op)					\
	Real##size Real##size##_mul##name 			\
		(Real##size r1, Real##size r2, Real##size r3) {	\
		return r1 * r2 op r3;				\
	}
ternary(32, add, +)
ternary(64, add, +)
ternary(32, sub, -)
ternary(64, sub, -)
#undef ternary
