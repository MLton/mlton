#include "platform.h"

void INetSock_toAddr (Pointer in_addr, Int port, Char* addr, Int *addrlen) {
        struct sockaddr_in *sa = (struct sockaddr_in*)addr;

        sa->sin_family = AF_INET;
        sa->sin_port = port;
        sa->sin_addr = *(struct in_addr*)in_addr;
        *addrlen = sizeof(struct sockaddr_in);
}

static int port;
static struct in_addr in_addr;

void INetSock_fromAddr (Char* addr) {
        struct sockaddr_in *sa = (struct sockaddr_in*)addr;

        assert(sa->sin_family == AF_INET);
        port = sa->sin_port;
        in_addr = sa->sin_addr;
}

Int INetSock_getPort (void) {
        return port;
}

void INetSock_getInAddr (Pointer addr) {
        *(struct in_addr*)addr = in_addr;
}
