#include "platform.h"

static struct tm Date_tmIn;
static struct tm *Date_tmOut;

C_Int_t Date_Tm_getHour(void) { return Date_tmOut->tm_hour; }
C_Int_t Date_Tm_getIsDst(void) { return Date_tmOut->tm_isdst; }
C_Int_t Date_Tm_getMDay(void) { return Date_tmOut->tm_mday; }
C_Int_t Date_Tm_getMin(void) { return Date_tmOut->tm_min; }
C_Int_t Date_Tm_getMon(void) { return Date_tmOut->tm_mon; }
C_Int_t Date_Tm_getSec(void) { return Date_tmOut->tm_sec; }
C_Int_t Date_Tm_getWDay(void) { return Date_tmOut->tm_wday; }
C_Int_t Date_Tm_getYDay(void) { return Date_tmOut->tm_yday; }
C_Int_t Date_Tm_getYear(void) { return Date_tmOut->tm_year; }

void Date_Tm_setHour(C_Int_t x) { Date_tmIn.tm_hour = x; }
void Date_Tm_setIsDst(C_Int_t x) { Date_tmIn.tm_isdst = x; }
void Date_Tm_setMDay(C_Int_t x) { Date_tmIn.tm_mday = x; }
void Date_Tm_setMin(C_Int_t x) { Date_tmIn.tm_min = x; }
void Date_Tm_setMon(C_Int_t x) { Date_tmIn.tm_mon = x; }
void Date_Tm_setSec(C_Int_t x) { Date_tmIn.tm_sec = x; }
void Date_Tm_setWDay(C_Int_t x) { Date_tmIn.tm_wday = x; }
void Date_Tm_setYDay(C_Int_t x) { Date_tmIn.tm_yday = x; }
void Date_Tm_setYear(C_Int_t x) { Date_tmIn.tm_year = x; }

C_Errno_t(C_Int_t) Date_gmTime(Ref(C_Time_t) p) {
  Date_tmOut = gmtime((time_t*)p);
  if (Date_tmOut == NULL) return -1;
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
  Date_tmOut = localtime((time_t*)p);
  if (Date_tmOut == NULL) return -1;
  return 0;
}

C_Errno_t(C_Time_t) Date_mkTime(void) {
  return mktime(&Date_tmIn);
}

C_Size_t Date_strfTime(Array(Char8_t) buf, C_Size_t n, NullString8_t fmt) {
  return strftime((char*)(buf), n, (const char*)(fmt), &Date_tmIn);
}
