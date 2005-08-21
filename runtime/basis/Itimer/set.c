#include "platform.h"

void Itimer_set (Int which,
                        Int interval_tv_sec, Int interval_tv_usec,
                        Int value_tv_sec, Int value_tv_usec) {
        struct itimerval        v;
        int i;

        v.it_interval.tv_sec = interval_tv_sec;
        v.it_interval.tv_usec = interval_tv_usec;
        v.it_value.tv_sec = value_tv_sec;
        v.it_value.tv_usec = value_tv_usec;
        i = setitimer (which, &v, (struct itimerval *)NULL);
        assert(i == 0);
}
