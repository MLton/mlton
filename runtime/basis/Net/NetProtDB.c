#include "platform.h"

static struct protoent *protoent;

Cstring NetProtDB_Entry_name() {
	return (Cstring)protoent->p_name;
}

Int NetProtDB_Entry_numAliases() {
	int num = 0;
	while (protoent->p_aliases[num] != NULL) num++;
	return num;
}

Cstring NetProtDB_Entry_aliasesN(Int n) {
	return (Cstring)protoent->p_aliases[n];
}

Int NetProtDB_Entry_protocol() {
	return protoent->p_proto;
}

Int NetProtDB_getByName(Cstring name) {
	protoent = getprotobyname((char*)name);
	return (protoent != NULL and protoent->p_name != NULL);
}

Int NetProtDB_getByNumber(Int proto) {
	protoent = getprotobynumber(proto);
	return (protoent != NULL and protoent->p_name != NULL);
}
