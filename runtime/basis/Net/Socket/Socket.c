#include "platform.h"

C_Errno_t(C_Int_t) Socket_accept (C_Sock_t s, Array(Word8_t) addr, Ref(C_Socklen_t) addrlen) {
  int out;
  
  MLton_initSockets ();
  out = accept (s, (struct sockaddr*)addr, (socklen_t*)addrlen);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_Int_t) Socket_bind (C_Sock_t s, Vector(Word8_t) addr, C_Socklen_t addrlen) {
  int out;
  
  MLton_initSockets ();
  out = bind (s, (const struct sockaddr*)addr, (socklen_t)addrlen);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_Int_t) Socket_close(C_Sock_t s) {
#ifdef __MINGW32__
  int out;
  
  MLton_initSockets ();
  out = closesocket(s);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
#else
  return close(s);
#endif
}

C_Errno_t(C_Int_t) Socket_connect (C_Sock_t s, Vector(Word8_t) addr, C_Socklen_t addrlen) {
  int out;
  
  MLton_initSockets ();
  out = connect (s, (const struct sockaddr*)addr, (socklen_t)addrlen);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Int_t Socket_familyOfAddr(Vector(Word8_t) addr) {
  return ((const struct sockaddr*)addr)->sa_family;
}

C_Errno_t(C_Int_t) Socket_listen (C_Sock_t s, C_Int_t backlog) {
  int out;
  
  MLton_initSockets ();
  out = listen (s, backlog);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_SSize_t) 
Socket_recv (C_Sock_t s, Array(Word8_t) msg, 
             C_Int_t start, C_Size_t len, C_Int_t flags) {
  ssize_t out;
  
  MLton_initSockets ();
  out = MLton_recv (s, (void*)((char *)msg + start), len, flags);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_SSize_t) 
Socket_recvFrom (C_Sock_t s, Array(Word8_t) msg, 
                 C_Int_t start, C_Size_t len, C_Int_t flags,
                 Array(Word8_t) addr, Ref(C_Socklen_t) addrlen) {
  ssize_t out;
  
  MLton_initSockets ();
  out = MLton_recvfrom (s, (void*)((char *)msg + start), len, flags,
                        (struct sockaddr*)addr, (socklen_t*)addrlen);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

static inline C_Errno_t(C_SSize_t)
Socket_send (C_Sock_t s, Pointer msg, 
             C_Int_t start, C_Size_t len, C_Int_t flags) {
  ssize_t out;
  
  MLton_initSockets ();
  out = send (s, (void*)((char *)msg + start), len, flags);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_SSize_t)
Socket_sendArr (C_Sock_t s, Array(Word8_t) msg, 
                C_Int_t start, C_Size_t len, C_Int_t flags) {
  return Socket_send (s, (Pointer)msg, start, len, flags);
}
C_Errno_t(C_SSize_t)
Socket_sendVec (C_Sock_t s, Vector(Word8_t) msg, 
                C_Int_t start, C_Size_t len, C_Int_t flags) {
  return Socket_send (s, (Pointer)msg, start, len, flags);
}

static inline C_Errno_t(C_SSize_t) 
Socket_sendTo (C_Sock_t s, Pointer msg, 
               C_Int_t start, C_Size_t len, C_Int_t flags,
               Vector(Word8_t) addr, C_Socklen_t addrlen) {
  ssize_t out;
  
  MLton_initSockets ();
  out = sendto (s, (void*)((char *)msg + start), len, flags,
                (const struct sockaddr*)addr, (socklen_t)addrlen);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_SSize_t) 
Socket_sendArrTo (C_Sock_t s, Array(Word8_t) msg, 
                  C_Int_t start, C_Size_t len, C_Int_t flags,
                  Vector(Word8_t) addr, C_Socklen_t addrlen) {
  return Socket_sendTo (s, (Pointer)msg, start, len, flags, addr, addrlen);
}
C_Errno_t(C_SSize_t) 
Socket_sendVecTo (C_Sock_t s, Vector(Word8_t) msg, 
                  C_Int_t start, C_Size_t len, C_Int_t flags,
                  Vector(Word8_t) addr, C_Socklen_t addrlen) {
  return Socket_sendTo (s, (Pointer)msg, start, len, flags, addr, addrlen);
}

C_Errno_t(C_Int_t) Socket_shutdown (C_Sock_t s, C_Int_t how) {
  int out;
  
  MLton_initSockets ();
  out = shutdown (s, how);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_Int_t) 
Socket_Ctl_getSockOptC_Int (C_Sock_t s, C_Int_t level, C_Int_t optname,
                            Ref(C_Int_t) optval) {
  socklen_t optlen = sizeof(int);
  int out;
  
  MLton_initSockets ();
  out = getsockopt (s, level, optname, optval, &optlen);
  assert (optlen == sizeof(int));
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_Int_t)
Socket_Ctl_setSockOptC_Int (C_Sock_t s, C_Int_t level, C_Int_t optname,
                            C_Int_t optval) {
  socklen_t optlen = sizeof(int);
  int out;
  
  MLton_initSockets ();
  out = setsockopt (s, level, optname, &optval, optlen);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_Int_t)
Socket_Ctl_getSockOptC_Linger (C_Sock_t s, C_Int_t level, C_Int_t optname,
                               Ref(C_Int_t) optval_l_onoff, Ref(C_Int_t) optval_l_linger) {
  struct linger optval;
  socklen_t optlen = sizeof(struct linger);
  int out;

  MLton_initSockets ();
  out = getsockopt (s, level, optname, &optval, &optlen);
  assert (optlen == sizeof(struct linger));
  *((int*)optval_l_onoff) = optval.l_onoff;
  *((int*)optval_l_linger) = optval.l_linger;
  if (out == -1) MLton_fixSocketErrno ();

  return out;
}

C_Errno_t(C_Int_t)
Socket_Ctl_setSockOptC_Linger (C_Sock_t s, C_Int_t level, C_Int_t optname,
                               C_Int_t optval_l_onoff, C_Int_t optval_l_linger) {
  struct linger optval;
  socklen_t optlen = sizeof(struct linger);
  int out;

  MLton_initSockets ();
  optval.l_onoff = optval_l_onoff;
  optval.l_linger = optval_l_linger;
  out = setsockopt (s, level, optname, &optval, optlen);
  if (out == -1) MLton_fixSocketErrno ();

  return out;
}

C_Errno_t(C_Int_t) Socket_Ctl_getPeerName (C_Sock_t s, Array(Word8_t) name, Ref(C_Socklen_t) namelen) {
  int out;
  
  MLton_initSockets ();
  out = getpeername (s, (struct sockaddr*)name, (socklen_t*)namelen);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_Int_t) Socket_Ctl_getSockName (C_Sock_t s, Array(Word8_t) name, Ref(C_Socklen_t) namelen) {
  int out;
  
  MLton_initSockets ();
  out = getsockname (s, (struct sockaddr*)name, (socklen_t*)namelen);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_Int_t)
Socket_Ctl_getNREAD (C_Sock_t s, Ref(C_Int_t) argp) {
  int out;

  out = ioctl (s, FIONREAD, &argp);
  if (out == -1) MLton_fixSocketErrno ();

  return out;
}

C_Errno_t(C_Int_t)
Socket_Ctl_getATMARK (C_Sock_t s, Ref(C_Int_t) argp) {
  int out;

  out = ioctl (s, SIOCATMARK, &argp);
  if (out == -1) MLton_fixSocketErrno ();

  return out;
}
