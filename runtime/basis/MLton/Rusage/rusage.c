#include "platform.h"

static struct rusage MLton_Rusage_self;
static struct rusage MLton_Rusage_children;
static struct rusage MLton_Rusage_gc;

C_Time_t MLton_Rusage_self_utime_sec (void) {
  return MLton_Rusage_self.ru_utime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_self_utime_usec (void) {
  return MLton_Rusage_self.ru_utime.tv_usec;
}

C_Time_t MLton_Rusage_self_stime_sec (void) {
  return MLton_Rusage_self.ru_stime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_self_stime_usec (void) {
  return MLton_Rusage_self.ru_stime.tv_usec;
}

C_Time_t MLton_Rusage_children_utime_sec (void) {
  return MLton_Rusage_children.ru_utime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_children_utime_usec (void) {
  return MLton_Rusage_children.ru_utime.tv_usec;
}

C_Time_t MLton_Rusage_children_stime_sec (void) {
  return MLton_Rusage_children.ru_stime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_children_stime_usec (void) {
  return MLton_Rusage_children.ru_stime.tv_usec;
}

C_Time_t MLton_Rusage_gc_utime_sec (void) {
  return MLton_Rusage_gc.ru_utime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_gc_utime_usec (void) {
  return MLton_Rusage_gc.ru_utime.tv_usec;
}

C_Time_t MLton_Rusage_gc_stime_sec (void) {
  return MLton_Rusage_gc.ru_stime.tv_sec;
}

C_SUSeconds_t MLton_Rusage_gc_stime_usec (void) {
  return MLton_Rusage_gc.ru_stime.tv_usec;
}

void MLton_Rusage_getrusage (GCState_t s) {
  MLton_Rusage_gc = *(GC_getRusageGCAddr (s));
  getrusage (RUSAGE_SELF, &MLton_Rusage_self);
  getrusage (RUSAGE_CHILDREN, &MLton_Rusage_children);
}
