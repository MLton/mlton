#define _ISOC99_SOURCE
#define _BSD_SOURCE

#include "platform.h"

static struct servent *servent;

C_String_t NetServDB_getEntryName(void) {
  return (C_String_t)(servent->s_name);
}

C_Int_t NetServDB_getEntryAliasesNum(void) {
  int num = 0;
  while (servent->s_aliases[num] != NULL) num++;
  return num;
}

C_String_t NetServDB_getEntryAliasesN(C_Int_t n) {
  return (C_String_t)(servent->s_aliases[n]);
}

C_Int_t NetServDB_getEntryPort(void) {
  return servent->s_port;
}

C_String_t NetServDB_getEntryProto(void) {
  return (C_String_t)(servent->s_proto);
}

C_Int_t NetServDB_getByName(NullString8_t name, NullString8_t proto) {
  servent = getservbyname((const char*)name, (const char*)proto);
  return (C_Int_t)(servent != NULL and servent->s_name != NULL);
}

C_Int_t NetServDB_getByNameNull(NullString8_t name) {
  return NetServDB_getByName(name, (NullString8_t)NULL);
}

C_Int_t NetServDB_getByPort(C_Int_t port, NullString8_t proto) {
  servent = getservbyport(port, (const char*)proto);
  return (C_Int_t)(servent != NULL and servent->s_name != NULL);
}

C_Int_t NetServDB_getByPortNull(C_Int_t port) {
  return NetServDB_getByPort(port, (NullString8_t)NULL);
}
