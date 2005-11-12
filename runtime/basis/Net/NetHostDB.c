#include "platform.h"

static struct hostent *hostent;

Cstring NetHostDB_Entry_name(void) {
        return (Cstring)hostent->h_name;
}

Int NetHostDB_Entry_numAliases(void) {
        int num = 0;
        while (hostent->h_aliases[num] != NULL) num++;
        return num;
}

Cstring NetHostDB_Entry_aliasesN(Int n) {
        return (Cstring)hostent->h_aliases[n];
}

Int NetHostDB_Entry_addrType(void) {
        return hostent->h_addrtype;
}

Int NetHostDB_Entry_length(void) {
        return hostent->h_length;
}

Int NetHostDB_Entry_numAddrs(void) {
        int num = 0;
        while (hostent->h_addr_list[num] != NULL) num++;
        return num;
}

void NetHostDB_Entry_addrsN(Int n, Pointer addr) {
        int i;
        for (i = 0; i < hostent->h_length; i++) {
                addr[i] = hostent->h_addr_list[n][i];
        }
        return;
}

Bool NetHostDB_getByAddress(Pointer addr, Int len) {
        hostent = gethostbyaddr(addr, len, AF_INET);
        return (hostent != NULL and hostent->h_name != NULL);
}

Bool NetHostDB_getByName(Cstring name) {
        hostent = gethostbyname((char*)name);
        return (hostent != NULL and hostent->h_name != NULL);
}

Int NetHostDB_getHostName(Pointer buf, Int len) {
        return (gethostname ((char*) buf, len));
}
