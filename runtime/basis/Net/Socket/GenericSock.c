#include "platform.h"

Int GenericSock_socket (Int domain, Int type, Int protocol) {
        MLton_initSockets ();
        return socket (domain, type, protocol);
}

Int GenericSocket_socketPair (Int domain, Int type, Int protocol, Int sv[2]) {
        MLton_initSockets ();
        return socketpair (domain, type, protocol, (int*)sv);
}
