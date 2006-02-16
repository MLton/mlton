#include "platform.h"

double frexp (double x, int* exp);

Real64 Real64_frexp (Real64 x, Int *exp) {
        int exp_;
        Real64 res;
        res = frexp (x, &exp_);
        *exp = exp_;
        return res;
}
