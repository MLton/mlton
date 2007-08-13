#include "platform.h"

const C_Int_t MLton_Syslog_Logopt_LOG_CONS = LOG_CONS;
const C_Int_t MLton_Syslog_Logopt_LOG_NDELAY = LOG_NDELAY;
const C_Int_t MLton_Syslog_Logopt_LOG_NOWAIT = LOG_NOWAIT;
const C_Int_t MLton_Syslog_Logopt_LOG_ODELAY = LOG_ODELAY;
/* NOT STANDARD */
#if (defined (LOG_PERROR))
const C_Int_t MLton_Syslog_Logopt_LOG_PERROR = LOG_PERROR;
#else
const C_Int_t MLton_Syslog_Logopt_LOG_PERROR = -1;
#endif
/* */
const C_Int_t MLton_Syslog_Logopt_LOG_PID = LOG_PID;

const C_Int_t MLton_Syslog_Facility_LOG_AUTH = LOG_AUTH;
const C_Int_t MLton_Syslog_Facility_LOG_CRON = LOG_CRON;
const C_Int_t MLton_Syslog_Facility_LOG_DAEMON = LOG_DAEMON;
const C_Int_t MLton_Syslog_Facility_LOG_KERN = LOG_KERN;
const C_Int_t MLton_Syslog_Facility_LOG_LOCAL0 = LOG_LOCAL0;
const C_Int_t MLton_Syslog_Facility_LOG_LOCAL1 = LOG_LOCAL1;
const C_Int_t MLton_Syslog_Facility_LOG_LOCAL2 = LOG_LOCAL2;
const C_Int_t MLton_Syslog_Facility_LOG_LOCAL3 = LOG_LOCAL3;
const C_Int_t MLton_Syslog_Facility_LOG_LOCAL4 = LOG_LOCAL4;
const C_Int_t MLton_Syslog_Facility_LOG_LOCAL5 = LOG_LOCAL5;
const C_Int_t MLton_Syslog_Facility_LOG_LOCAL6 = LOG_LOCAL6;
const C_Int_t MLton_Syslog_Facility_LOG_LOCAL7 = LOG_LOCAL7;
const C_Int_t MLton_Syslog_Facility_LOG_LPR = LOG_LPR;
const C_Int_t MLton_Syslog_Facility_LOG_MAIL = LOG_MAIL;
const C_Int_t MLton_Syslog_Facility_LOG_NEWS = LOG_NEWS;
/* NOT STANDARD */
#if (defined (LOG_SYSLOG))
const C_Int_t MLton_Syslog_Facility_LOG_SYSLOG = LOG_SYSLOG;
#else
const C_Int_t MLton_Syslog_Facility_LOG_SYSLOG = -1;
#endif
/* */
const C_Int_t MLton_Syslog_Facility_LOG_USER = LOG_USER;
const C_Int_t MLton_Syslog_Facility_LOG_UUCP = LOG_UUCP;

const C_Int_t MLton_Syslog_Severity_LOG_ALERT = LOG_ALERT;
const C_Int_t MLton_Syslog_Severity_LOG_CRIT = LOG_CRIT;
const C_Int_t MLton_Syslog_Severity_LOG_DEBUG = LOG_DEBUG;
const C_Int_t MLton_Syslog_Severity_LOG_EMERG = LOG_EMERG;
const C_Int_t MLton_Syslog_Severity_LOG_ERR = LOG_ERR;
const C_Int_t MLton_Syslog_Severity_LOG_INFO = LOG_INFO;
const C_Int_t MLton_Syslog_Severity_LOG_NOTICE = LOG_NOTICE;
const C_Int_t MLton_Syslog_Severity_LOG_WARNING = LOG_WARNING;
