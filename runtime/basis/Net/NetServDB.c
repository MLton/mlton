#include <netdb.h>
#include "mlton-basis.h"
#include "my-lib.h"

static struct servent *servent;

Cstring NetServDB_Entry_name() {
	return (Cstring)servent->s_name;
}

Int NetServDB_Entry_numAliases() {
	int num = 0;
	while (servent->s_aliases[num] != NULL) num++;
	return num;
}

Cstring NetServDB_Entry_aliasesN(Int n) {
	return (Cstring)servent->s_aliases[n];
}

Int NetServDB_Entry_port() {
	return servent->s_port;
}

Cstring NetServDB_Entry_protocol() {
	return (Cstring)servent->s_proto;
}

Int NetServDB_getByName(Cstring name, Cstring proto) {
	servent = getservbyname((char*)name, (char*)proto);
	return (servent != NULL and servent->s_name != NULL);
}

Int NetServDB_getByNameNull(Cstring name) {
	servent = getservbyname((char*)name, NULL);
	return (servent != NULL and servent->s_name != NULL);
}

Int NetServDB_getByPort(Int port, Cstring proto) {
	servent = getservbyport(port, (char*)proto);
	return (servent != NULL and servent->s_name != NULL);
}

Int NetServDB_getByPortNull(Int port) {
	servent = getservbyport(port, NULL);
	return (servent != NULL and servent->s_name != NULL);
}
