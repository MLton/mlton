#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

#include "gc.h"
#include "mlton-basis.h"

static struct rlimit rlimit;

Int MLton_Rlimit_get(Resource r)
{
	return getrlimit(r, &rlimit);
}

Rlimit MLton_Rlimit_getHard()
{
	return rlimit.rlim_max;
}

Rlimit MLton_Rlimit_getSoft()
{
	return rlimit.rlim_cur;
}

Int MLton_Rlimit_set(Resource r, Rlimit hard, Rlimit soft) 
{
	rlimit.rlim_max = hard;
	rlimit.rlim_cur = soft;
	return setrlimit(r, &rlimit);
}
