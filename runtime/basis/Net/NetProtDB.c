#include "platform.h"

static struct protoent *NetProtDB_protoent;

C_String_t NetProtDB_getEntryName(void) {
  return (C_String_t)(NetProtDB_protoent->p_name);
}

C_Int_t NetProtDB_getEntryAliasesNum(void) {
  int num = 0;
  while (NetProtDB_protoent->p_aliases[num] != NULL) num++;
  return num;
}

C_String_t NetProtDB_getEntryAliasesN(C_Int_t n) {
  return (C_String_t)(NetProtDB_protoent->p_aliases[n]);
}

C_Int_t NetProtDB_getEntryProto(void) {
  return NetProtDB_protoent->p_proto;
}

C_Int_t NetProtDB_getByName(NullString8_t name) {
  NetProtDB_protoent = getprotobyname((const char*)name);
  return (C_Int_t)(NetProtDB_protoent != NULL and NetProtDB_protoent->p_name != NULL);
}

C_Int_t NetProtDB_getByNumber(C_Int_t proto) {
  NetProtDB_protoent = getprotobynumber(proto);
  return (C_Int_t)(NetProtDB_protoent != NULL and NetProtDB_protoent->p_name != NULL);
}
