#include "platform.h"

Int Socket_accept (Int s, Char *addr, Int *addrlen) {
        MLton_initSockets ();
        return accept (s, (struct sockaddr*)addr, (socklen_t*)addrlen);
}

Int Socket_bind (Int s, Char *addr, Int addrlen) {
        MLton_initSockets ();
        return bind (s, (struct sockaddr*)addr, (socklen_t)addrlen);
}

Int Socket_close(Int s) {
        return close(s);
}

Int Socket_connect (Int s, Char *addr, Int addrlen) {
        MLton_initSockets ();
        return connect (s, (struct sockaddr*)addr, (socklen_t)addrlen);
}

Int Socket_familyOfAddr(Char *addr) {
        return ((struct sockaddr*)addr)->sa_family;
}

Int Socket_listen (Int s, Int backlog) {
        MLton_initSockets ();
        return listen (s, backlog);
}

Int Socket_recv (Int s, Char *msg, Int start, Int len, Word flags) {
        MLton_initSockets ();
        return recv (s, (void*)((char *)msg + start), (size_t)len, flags);
}

Int Socket_recvFrom (Int s, Char *msg, Int start, Int len, Word flags,
                    Char* addr, Int *addrlen) {
        MLton_initSockets ();
        return recvfrom (s, (void*)((char *)msg + start), (size_t)len, flags,
                                (struct sockaddr*)addr, (socklen_t*)addrlen);
}

Int Socket_send (Int s, Char *msg, Int start, Int len, Word flags) {
        MLton_initSockets ();
        return send (s, (void*)((char *)msg + start), (size_t)len, flags);
}

Int Socket_sendTo (Int s, Char *msg, Int start, Int len, Word flags,
                  Char* addr, Int addrlen) {
        MLton_initSockets ();
        return sendto (s, (void*)((char *)msg + start), (size_t)len, flags,
                      (struct sockaddr*)addr, (socklen_t)addrlen);
}

Int Socket_shutdown (Int s, Int how) {
        MLton_initSockets ();
        return shutdown (s, how);
}

Int GenericSock_socket (Int domain, Int type, Int protocol) {
        MLton_initSockets ();
        return socket (domain, type, protocol);
}

Int Socket_socketPair (Int domain, Int type, Int protocol, Int sv[2]) {
        MLton_initSockets ();
        return socketpair (domain, type, protocol, (int*)sv);
}

Int Socket_Ctl_getSockOpt (Int s, Int level, Int optname, Char *optval,
                                 Int *optlen) {
        MLton_initSockets ();
        return getsockopt (s, level, optname, (void*)optval, (socklen_t*)optlen);
}

Int Socket_Ctl_setSockOpt (Int s, Int level, Int optname, Char *optval, 
                                Int optlen) {
        MLton_initSockets ();
        return setsockopt (s, level, optname, (void*)optval, (socklen_t)optlen);
}

Int Socket_Ctl_getsetIOCtl (Int s, Int request, Char* argp) {
        MLton_initSockets ();
        return ioctl (s, request, argp);
}

Int Socket_Ctl_getPeerName (Int s, Char *name, Int *namelen) {
        MLton_initSockets ();
        return getpeername (s, (struct sockaddr*)name, (socklen_t*)namelen);
}

Int Socket_Ctl_getSockName (Int s, Char *name, Int *namelen) {
        MLton_initSockets ();
        return getsockname (s, (struct sockaddr*)name, (socklen_t*)namelen);
}
