#include "platform.h"

#ifndef DEBUG
#define DEBUG FALSE
#endif

static struct tm tm;
static struct tm *tmp;

C_Int_t Date_Tm_getHour(void) { return tmp->tm_hour; }
C_Int_t Date_Tm_getIsDst(void) { return tmp->tm_isdst; }
C_Int_t Date_Tm_getMDay(void) { return tmp->tm_mday; }
C_Int_t Date_Tm_getMin(void) { return tmp->tm_min; }
C_Int_t Date_Tm_getMon(void) { return tmp->tm_mon; }
C_Int_t Date_Tm_getSec(void) { return tmp->tm_sec; }
C_Int_t Date_Tm_getWDay(void) { return tmp->tm_wday; }
C_Int_t Date_Tm_getYDay(void) { return tmp->tm_yday; }
C_Int_t Date_Tm_getYear(void) { return tmp->tm_year; }

void Date_Tm_setHour(C_Int_t x) { tm.tm_hour = x; }
void Date_Tm_setIsDst(C_Int_t x) { tm.tm_isdst = x; }
void Date_Tm_setMDay(C_Int_t x) { tm.tm_mday = x; }
void Date_Tm_setMin(C_Int_t x) { tm.tm_min = x; }
void Date_Tm_setMon(C_Int_t x) { tm.tm_mon = x; }
void Date_Tm_setSec(C_Int_t x) { tm.tm_sec = x; }
void Date_Tm_setWDay(C_Int_t x) { tm.tm_wday = x; }
void Date_Tm_setYDay(C_Int_t x) { tm.tm_yday = x; }
void Date_Tm_setYear(C_Int_t x) { tm.tm_year = x; }

C_Errno_t(C_Int_t) Date_gmTime(Ref(C_Time_t) p) {
  tmp = gmtime((time_t*)p);
  if (DEBUG)
    fprintf (stderr, FMTPTR" = Date_gmTime ("FMTPTR")\n", 
             (uintptr_t)tmp, (uintptr_t)p);
  if (tmp == NULL) return -1;
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
  tmp = localtime((time_t*)p);
  if (DEBUG)
    fprintf (stderr, FMTPTR" = Date_localTime ("FMTPTR")\n", 
             (uintptr_t)tmp, (uintptr_t)p);
  if (tmp == NULL) return -1;
  return 0;
}

C_Errno_t(C_Time_t) Date_mkTime(void) {
  return mktime(&tm);
}

C_Size_t Date_strfTime(String_t buf, C_Size_t n, NullString_t fmt) {
  return strftime((char*)(buf), n, (char*)(fmt), &tm);
}
