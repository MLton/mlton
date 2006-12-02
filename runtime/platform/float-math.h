#define unaryReal(func) float func##f (float x);
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

#define binaryReal(func) float func##f (float x, float y);
binaryReal(atan2)
binaryReal(pow)
#undef binaryReal

float frexpf(float x, int *e);
float ldexpf (float x, int i);
