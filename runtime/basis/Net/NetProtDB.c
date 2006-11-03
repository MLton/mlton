#include "platform.h"

static struct protoent *protoent;

C_String_t NetProtDB_getEntryName(void) {
  return (C_String_t)(protoent->p_name);
}

C_Int_t NetProtDB_getEntryAliasesNum(void) {
  int num = 0;
  while (protoent->p_aliases[num] != NULL) num++;
  return num;
}

C_String_t NetProtDB_getEntryAliasesN(C_Int_t n) {
  return (C_String_t)(protoent->p_aliases[n]);
}

C_Int_t NetProtDB_getEntryProto(void) {
  return protoent->p_proto;
}

C_Int_t NetProtDB_getByName(NullString8_t name) {
  protoent = getprotobyname((const char*)name);
  return (C_Int_t)(protoent != NULL and protoent->p_name != NULL);
}

C_Int_t NetProtDB_getByNumber(C_Int_t proto) {
  protoent = getprotobynumber(proto);
  return (C_Int_t)(protoent != NULL and protoent->p_name != NULL);
}
