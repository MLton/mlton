#include "platform.h"

static struct rlimit rlimit;

C_Errno_t(C_Int_t) MLton_Rlimit_get (C_Int_t r) {
  return getrlimit (r, &rlimit);
}

C_RLim_t MLton_Rlimit_getHard (void) {
  return rlimit.rlim_max;
}

C_RLim_t MLton_Rlimit_getSoft (void) {
  return rlimit.rlim_cur;
}

C_Errno_t(C_Int_t) MLton_Rlimit_set (C_Int_t r, C_RLim_t hard, C_RLim_t soft) {
  rlimit.rlim_max = hard;
  rlimit.rlim_cur = soft;
  return setrlimit (r, &rlimit);
}
