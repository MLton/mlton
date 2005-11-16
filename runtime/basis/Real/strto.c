#include "platform.h"

Real32 gdtoa_strtof (char *s, char **endptr);
Real64 gdtoa_strtod (char *s, char **endptr);

Real32 Real32_strto (Pointer s) {
        char *endptr;
        Real32 res;

        res = gdtoa_strtof ((char *)s, &endptr);
        assert (NULL != endptr);
        return res;
}

Real64 Real64_strto (Pointer s) {
        char *endptr;
        Real64 res;

        res = gdtoa_strtod ((char *)s, &endptr);
        assert (NULL != endptr);
        return res;
}
