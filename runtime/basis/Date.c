#include "mlton-basis.h"

#include <stdio.h>
#include <time.h>

enum {
	DEBUG = 0,
};

static struct tm tm;
static struct tm *tmp;

Int Date_Tm_sec() {
	return tmp->tm_sec;
}

Int Date_Tm_min() {
	return tmp->tm_min;
}

Int Date_Tm_hour() {
	return tmp->tm_hour;
}

Int Date_Tm_mday() {
	return tmp->tm_mday;
}

Int Date_Tm_mon() {
	return tmp->tm_mon;
}

Int Date_Tm_year() {
	return tmp->tm_year;
}

Int Date_Tm_wday() {
	return tmp->tm_wday;
}

Int Date_Tm_yday() {
	return tmp->tm_yday;
}

Int Date_Tm_isdst() {
	return tmp->tm_isdst;
}

void Date_Tm_setSec(Int x) {
	tm.tm_sec = x;
}

void Date_Tm_setMin(Int x) {
	tm.tm_min = x;
}

void Date_Tm_setHour(Int x) {
	tm.tm_hour = x;
}

void Date_Tm_setMday(Int x) {
	tm.tm_mday = x;
}

void Date_Tm_setMon(Int x) {
	tm.tm_mon = x;
}

void Date_Tm_setYear(Int x) {
	tm.tm_year = x;
}

void Date_Tm_setWday(Int x) {
	tm.tm_wday = x;
}

void Date_Tm_setYday(Int x) {
	tm.tm_yday = x;
}

void Date_Tm_setIsdst(Int x) {
	tm.tm_isdst = x;
}

Cstring Date_ascTime() {
	return (Cstring)asctime(&tm);
}

void Date_gmTime(Pointer p) {
	tmp = gmtime((time_t*)p);
}

/* The idea for Date_localOffset comes from KitV3 src/Runtime/Time.c */
Int Date_localOffset() {
	time_t t1, t2;

	t1 = time(NULL);
	t2 = mktime(gmtime(&t1));
	return difftime(t2, t1);
}

void Date_localTime(Pointer p) {
	tmp = localtime((time_t*)p);
	if (DEBUG)
		fprintf (stderr, "0x%08x = Date_localTime (0x%08x)\n", 
				(unsigned int)tmp, (unsigned int)p);
}

Int Date_mkTime() {
	return mktime(&tm);
}

Int Date_strfTime(Pointer buf, Int n, NullString fmt) {
	return strftime((char*)(buf), n, (char*)(fmt), &tm);
}
