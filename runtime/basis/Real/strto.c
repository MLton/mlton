#include "platform.h"

Real32 gdtoa_strtof (char *s, char **endptr);
Real64 gdtoa_strtod (char *s, char **endptr);

Real32 Real32_strto (char *s) {
        char *endptr;
        Real32 res;

        res = gdtoa_strtof (s, &endptr);
        assert (NULL != endptr);
        return res;
}

Real64 Real64_strto (char *s) {
        char *endptr;
        Real64 res;

        res = gdtoa_strtod (s, &endptr);
        assert (NULL != endptr);
        return res;
}
