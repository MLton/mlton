#if (defined (__FreeBSD__))
#include <sys/time.h>
#endif
#include <sys/resource.h>

#include "gc.h"
#include "mlton-basis.h"

extern struct GC_state gcState;

static struct rusage self;
static struct rusage children;
static struct rusage gc;

Int MLton_Rusage_self_utime_sec() {
  return self.ru_utime.tv_sec;
}

Int MLton_Rusage_self_utime_usec() {
  return self.ru_utime.tv_usec;
}

Int MLton_Rusage_self_stime_sec() {
  return self.ru_stime.tv_sec;
}

Int MLton_Rusage_self_stime_usec() {
  return self.ru_stime.tv_usec;
}

Int MLton_Rusage_children_utime_sec() {
  return children.ru_utime.tv_sec;
}

Int MLton_Rusage_children_utime_usec() {
  return children.ru_utime.tv_usec;
}

Int MLton_Rusage_children_stime_sec() {
  return children.ru_stime.tv_sec;
}

Int MLton_Rusage_children_stime_usec() {
  return children.ru_stime.tv_usec;
}

Int MLton_Rusage_gc_utime_sec() {
  return gc.ru_utime.tv_sec;
}

Int MLton_Rusage_gc_utime_usec() {
  return gc.ru_utime.tv_usec;
}

Int MLton_Rusage_gc_stime_sec() {
  return gc.ru_stime.tv_sec;
}

Int MLton_Rusage_gc_stime_usec() {
  return gc.ru_stime.tv_usec;
}

void MLton_Rusage_ru() {
	gc = gcState.ru_gc;
	fixedGetrusage(RUSAGE_SELF, &self);
	fixedGetrusage(RUSAGE_CHILDREN, &children);
}
