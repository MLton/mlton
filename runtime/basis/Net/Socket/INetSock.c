#include "platform.h"

void 
Socket_INetSock_toAddr (Vector(Word8_t) in_addr, C_Int_t port, 
                        Array(Word8_t) addr, Ref(C_Socklen_t) addrlen) {
  struct sockaddr_in *sa = (struct sockaddr_in*)addr;

  sa->sin_family = AF_INET;
  sa->sin_port = port;
  sa->sin_addr = *(struct in_addr*)in_addr;
  *((socklen_t*)addrlen) = sizeof(struct sockaddr_in);
}

static int port;
static struct in_addr in_addr;

void Socket_INetSock_fromAddr (Vector(Word8_t) addr) {
  struct sockaddr_in *sa = (struct sockaddr_in*)addr;
  
  assert(sa->sin_family == AF_INET);
  port = sa->sin_port;
  in_addr = sa->sin_addr;
}

Int Socket_INetSock_getPort (void) {
  return port;
}

void Socket_INetSock_getInAddr (Array(Word8_t) addr) {
  *(struct in_addr*)addr = in_addr;
}
