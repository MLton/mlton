#include "platform.h"

static struct rlimit MLton_RLimit_rlimit;

C_Errno_t(C_Int_t) MLton_Rlimit_get (C_Int_t r) {
  return getrlimit (r, &MLton_RLimit_rlimit);
}

C_RLim_t MLton_Rlimit_getHard (void) {
  return MLton_RLimit_rlimit.rlim_max;
}

C_RLim_t MLton_Rlimit_getSoft (void) {
  return MLton_RLimit_rlimit.rlim_cur;
}

C_Errno_t(C_Int_t) MLton_Rlimit_set (C_Int_t r, C_RLim_t hard, C_RLim_t soft) {
  MLton_RLimit_rlimit.rlim_max = hard;
  MLton_RLimit_rlimit.rlim_cur = soft;
  return setrlimit (r, &MLton_RLimit_rlimit);
}
