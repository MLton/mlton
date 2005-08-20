#include "platform.h"

#define unaryReal(f, g)                                                \
        Real64 g (Real64 x);                                        \
        Real64 Real64_##f (Real64 x) {                \
                return g (x);                                        \
        }                                                        \
        Real32 Real32_##f (Real32 x) {                \
                return (Real32)(Real64_##f ((Real64)x));        \
        }
unaryReal(abs, fabs)
unaryReal(round, rint)
#undef unaryReal

#define binaryReal(f, g)                                                        \
        Real64 g (Real64 x, Real64 y);                                                \
        Real64 Real64_Math_##f (Real64 x, Real64 y) {                \
                return g (x, y);                                                \
        }                                                                        \
        Real32 Real32_Math_##f (Real32 x, Real32 y) {                \
                return (Real32)(Real64_Math_##f ((Real64)x, (Real64)y));        \
        }
binaryReal(atan2, atan2)
#undef binaryReal

#define unaryReal(f, g)                                                \
        Real64 g (Real64 x);                                        \
        Real64 Real64_Math_##f (Real64 x) {        \
                return g (x);                                        \
        }                                                        \
        Real32 Real32_Math_##f (Real32 x) {        \
                return (Real32)(Real64_Math_##f ((Real64)x));        \
        }
unaryReal(acos, acos)
unaryReal(asin, asin)
unaryReal(atan, atan)
unaryReal(cos, cos)
unaryReal(exp, exp)
unaryReal(ln, log)
unaryReal(log10, log10)
unaryReal(sin, sin)
unaryReal(sqrt, sqrt)
unaryReal(tan, tan)
#undef unaryReal

double ldexp (double x, int i);
Real64 Real64_ldexp (Real64 x, Int32 i) {
        return ldexp (x, i);
}

Real32 Real32_ldexp (Real32 x, Int32 i) {
        return (Real32)Real64_ldexp ((Real64)x, i);
}
