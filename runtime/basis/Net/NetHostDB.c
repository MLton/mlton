#include "platform.h"

static struct hostent *NetHostDB_hostent;

C_String_t NetHostDB_getEntryName(void) {
  return (C_String_t)(NetHostDB_hostent->h_name);
}

C_Int_t NetHostDB_getEntryAliasesNum(void) {
  int num = 0;
  while (NetHostDB_hostent->h_aliases[num] != NULL) num++;
  return num;
}

C_String_t NetHostDB_getEntryAliasesN(C_Int_t n) {
  return (C_String_t)(NetHostDB_hostent->h_aliases[n]);
}

C_Int_t NetHostDB_getEntryAddrType(void) {
  return NetHostDB_hostent->h_addrtype;
}

C_Int_t NetHostDB_getEntryLength(void) {
  return NetHostDB_hostent->h_length;
}

C_Int_t NetHostDB_getEntryAddrsNum(void) {
  int num = 0;
  while (NetHostDB_hostent->h_addr_list[num] != NULL) num++;
  return num;
}

void NetHostDB_getEntryAddrsN(C_Int_t n, Array(Word8_t) addr) {
  int i;
  for (i = 0; i < NetHostDB_hostent->h_length; i++) {
    ((char*)addr)[i] = NetHostDB_hostent->h_addr_list[n][i];
  }
  return;
}

C_Int_t NetHostDB_getByAddress(Vector(Word8_t) addr, C_Socklen_t len) {
  MLton_initSockets ();
  NetHostDB_hostent = gethostbyaddr((const char*)addr, len, AF_INET);
  return (C_Int_t)(NetHostDB_hostent != NULL and NetHostDB_hostent->h_name != NULL);
}

C_Int_t NetHostDB_getByName(NullString8_t name) {
  MLton_initSockets ();
  NetHostDB_hostent = gethostbyname((const char*)name);
  return (C_Int_t)(NetHostDB_hostent != NULL and NetHostDB_hostent->h_name != NULL);
}

C_Errno_t(C_Int_t) NetHostDB_getHostName(Array(Char8_t) buf, C_Size_t len) {
  int out;
  
  MLton_initSockets ();
  out = gethostname ((char*)buf, len);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}
