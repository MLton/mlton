#include "platform.h"

C_Errno_t(C_Int_t) 
Socket_GenericSock_socket (C_Int_t domain, C_Int_t type, C_Int_t protocol) {
  int out;
  
  MLton_initSockets ();
  out = socket (domain, type, protocol);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_Int_t)
Socket_GenericSock_socketPair (C_Int_t domain, C_Int_t type, C_Int_t protocol, Array(C_Int_t) sv) {
  int out;
  
  MLton_initSockets ();
  out = socketpair (domain, type, protocol, (int*)sv);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}
