#include "platform.h"

static struct hostent *hostent;

C_String_t NetHostDB_getEntryName(void) {
  return (C_String_t)(hostent->h_name);
}

C_Int_t NetHostDB_getEntryAliasesNum(void) {
  int num = 0;
  while (hostent->h_aliases[num] != NULL) num++;
  return num;
}

C_String_t NetHostDB_getEntryAliasesN(C_Int_t n) {
  return (C_String_t)(hostent->h_aliases[n]);
}

C_Int_t NetHostDB_getEntryAddrType(void) {
  return hostent->h_addrtype;
}

C_Int_t NetHostDB_getEntryLength(void) {
  return hostent->h_length;
}

C_Int_t NetHostDB_getEntryAddrsNum(void) {
  int num = 0;
  while (hostent->h_addr_list[num] != NULL) num++;
  return num;
}

void NetHostDB_getEntryAddrsN(C_Int_t n, Array(C_Char_t) addr) {
  int i;
  for (i = 0; i < hostent->h_length; i++) {
    ((char*)addr)[i] = hostent->h_addr_list[n][i];
  }
  return;
}

Bool_t NetHostDB_getByAddress(Vector(C_Char_t) addr, C_Socklen_t len) {
  hostent = gethostbyaddr((void*)addr, len, AF_INET);
  return (hostent != NULL and hostent->h_name != NULL);
}

Bool_t NetHostDB_getByName(NullString8_t name) {
  hostent = gethostbyname((char*)name);
  return (hostent != NULL and hostent->h_name != NULL);
}

Bool NetHostDB_getHostName(Array(Char8_t) buf, C_Size_t len) {
  return (gethostname ((char*)buf, len) == 0);
}
