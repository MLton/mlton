#include "platform.h"

#define UNIXSOCK_PATH_MAX (sizeof(struct sockaddr_un) - offsetof(struct sockaddr_un, sun_path))

void Socket_UnixSock_toAddr (NullString8_t path, C_Size_t pathlen, Array(Word8_t) addr, Ref(C_Socklen_t) addrlen) {
  size_t i;
  struct sockaddr_un *sa = (struct sockaddr_un*)addr;

  sa->sun_family = AF_UNIX;
  if (pathlen <= UNIXSOCK_PATH_MAX) {
    for (i = 0; i < pathlen; i++) {
      sa->sun_path[i] = ((const char*)path)[i];
    }
  } else {
    for (i = 0; i < UNIXSOCK_PATH_MAX-1; i++) {
      sa->sun_path[i] = ((const char*)path)[i];
    }
    sa->sun_path[UNIXSOCK_PATH_MAX-1] = '\000';
  }
  *((socklen_t*)addrlen) = sizeof(struct sockaddr_un);
}

C_Size_t Socket_UnixSock_pathLen (Vector(Word8_t) addr) {
  size_t i;
  const struct sockaddr_un *sa = (const struct sockaddr_un*)addr;

  i = 0;
  if (sa->sun_path[i] == '\000') {
    return UNIXSOCK_PATH_MAX;
  } else {
    while (i < UNIXSOCK_PATH_MAX && sa->sun_path[i] != '\000') i++;
    return i;
  }
}

void Socket_UnixSock_fromAddr (Vector(Word8_t) addr, Array(Char8_t) path, C_Size_t pathlen) {
  size_t i;
  const struct sockaddr_un *sa = (const struct sockaddr_un*)addr;

  assert (sa->sun_family == AF_UNIX);
  for (i = 0; i < pathlen; i++) {
    ((char*)path)[i] = sa->sun_path[i];
  }
}

#undef UNIXSOCK_PATH_MAX
