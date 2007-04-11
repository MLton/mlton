#include "platform.h"

static struct tm tm_in;
static struct tm *tm_out;

C_Int_t Date_Tm_getHour(void) { return tm_out->tm_hour; }
C_Int_t Date_Tm_getIsDst(void) { return tm_out->tm_isdst; }
C_Int_t Date_Tm_getMDay(void) { return tm_out->tm_mday; }
C_Int_t Date_Tm_getMin(void) { return tm_out->tm_min; }
C_Int_t Date_Tm_getMon(void) { return tm_out->tm_mon; }
C_Int_t Date_Tm_getSec(void) { return tm_out->tm_sec; }
C_Int_t Date_Tm_getWDay(void) { return tm_out->tm_wday; }
C_Int_t Date_Tm_getYDay(void) { return tm_out->tm_yday; }
C_Int_t Date_Tm_getYear(void) { return tm_out->tm_year; }

void Date_Tm_setHour(C_Int_t x) { tm_in.tm_hour = x; }
void Date_Tm_setIsDst(C_Int_t x) { tm_in.tm_isdst = x; }
void Date_Tm_setMDay(C_Int_t x) { tm_in.tm_mday = x; }
void Date_Tm_setMin(C_Int_t x) { tm_in.tm_min = x; }
void Date_Tm_setMon(C_Int_t x) { tm_in.tm_mon = x; }
void Date_Tm_setSec(C_Int_t x) { tm_in.tm_sec = x; }
void Date_Tm_setWDay(C_Int_t x) { tm_in.tm_wday = x; }
void Date_Tm_setYDay(C_Int_t x) { tm_in.tm_yday = x; }
void Date_Tm_setYear(C_Int_t x) { tm_in.tm_year = x; }

C_Errno_t(C_Int_t) Date_gmTime(Ref(C_Time_t) p) {
  tm_out = gmtime((time_t*)p);
  if (tm_out == NULL) return -1;
  return 0;
}

/* The idea for Date_localOffset comes from KitV3 src/Runtime/Time.c */
C_Double_t Date_localOffset(void) {
  time_t t1, t2;

  t1 = time(NULL);
  t2 = mktime(gmtime(&t1));
  return difftime(t2, t1);
}

C_Errno_t(C_Int_t) Date_localTime(Ref(C_Time_t) p) {
  tm_out = localtime((time_t*)p);
  if (tm_out == NULL) return -1;
  return 0;
}

C_Errno_t(C_Time_t) Date_mkTime(void) {
  return mktime(&tm_in);
}

C_Size_t Date_strfTime(Array(Char8_t) buf, C_Size_t n, NullString8_t fmt) {
  return strftime((char*)(buf), n, (const char*)(fmt), &tm_in);
}
