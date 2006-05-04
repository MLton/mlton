#include "platform.h"

void MLton_Syslog_closelog(void) {
  closelog();
}

/* openlog relies on the string being around forever. */
void MLton_Syslog_openlog(NullString8_t s, C_Int_t o, C_Int_t f) {
  const char *s_ = strdup ((const char*)s);
  if (s_ == NULL)
    s_ = "";
  openlog (s_, o, f);
}

void MLton_Syslog_syslog(C_Int_t p, NullString8_t s) {
  syslog(p, "%s", (const char*)s);
}
