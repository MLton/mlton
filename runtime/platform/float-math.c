#define unaryReal(func)                         \
float func##f (float x) {                       \
        return (float)(func((double)x));        \
}
unaryReal(acos)
unaryReal(asin)
unaryReal(atan)
unaryReal(cos)
unaryReal(cosh)
unaryReal(exp)
unaryReal(fabs)
unaryReal(log)
unaryReal(log10)
unaryReal(rint)
unaryReal(sin)
unaryReal(sinh)
unaryReal(sqrt)
unaryReal(tan)
unaryReal(tanh)
#undef unaryReal

#define binaryReal(func)                                \
float func##f (float x, float y) {                      \
        return (float)(func((double)x, (double)y));     \
}
binaryReal(atan2)
binaryReal(nextafter)
binaryReal(pow)
#undef binaryReal

float frexpf(float x, int *exp) {
        return (float)frexp((double)x, exp);
}

float ldexpf (float x, int i) {
        return (float)ldexp((double)x, i);
}
