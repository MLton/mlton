#include "platform.h"

C_Errno_t(C_Int_t) 
Socket_GenericSock_socket (C_Int_t domain, C_Int_t type, C_Int_t protocol) {
  MLton_initSockets ();
  return socket (domain, type, protocol);
}

C_Errno_t(C_Int_t)
Socket_GenericSock_socketPair (C_Int_t domain, C_Int_t type, C_Int_t protocol, Array(C_Int_t) sv) {
  MLton_initSockets ();
  return socketpair (domain, type, protocol, (int*)sv);
}
