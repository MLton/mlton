#include "platform.h"

static struct rlimit rlimit;

Int MLton_Rlimit_get (Resource r) {
	return getrlimit (r, &rlimit);
}

Rlimit MLton_Rlimit_getHard () {
	return rlimit.rlim_max;
}

Rlimit MLton_Rlimit_getSoft () {
	return rlimit.rlim_cur;
}

Int MLton_Rlimit_set (Resource r, Rlimit hard, Rlimit soft) {
	rlimit.rlim_max = hard;
	rlimit.rlim_cur = soft;
	return setrlimit (r, &rlimit);
}
