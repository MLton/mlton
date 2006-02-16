#include "platform.h"

#define UNIX_PATH_MAX 108

void UnixSock_toAddr (Char* path, Int pathlen, Char* addr, Int *addrlen) {
        int i;
        struct sockaddr_un *sa = (struct sockaddr_un*)addr;

        sa->sun_family = AF_UNIX;
        i = 0;
        if (pathlen <= UNIX_PATH_MAX) {
                for (i = 0; i < pathlen; i++) {
                        sa->sun_path[i] = path[i];
                }
        } else {
                for (i = 0; i < UNIX_PATH_MAX-1; i++) {
                        sa->sun_path[i] = path[i];
                }
                sa->sun_path[UNIX_PATH_MAX-1] = '\000';
        }
        *addrlen = sizeof(struct sockaddr_un);
}

Int UnixSock_pathLen (Char* addr) {
        int i;
        struct sockaddr_un *sa = (struct sockaddr_un*)addr;

        i = 0;
        if (sa->sun_path[i] == '\000') {
                return UNIX_PATH_MAX;
        } else {
                while (i < UNIX_PATH_MAX && sa->sun_path[i] != '\000') i++;
                return i;
        }
}

void UnixSock_fromAddr (Char* addr, Char* path, Int pathlen) {
        int i;
        struct sockaddr_un *sa = (struct sockaddr_un*)addr;

        assert (sa->sun_family == AF_UNIX);
        for (i = 0; i < pathlen; i++) {
                path[i] = sa->sun_path[i];
        }
}
