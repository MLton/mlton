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
  int out;
  
  MLton_initSockets ();
  out = MLton_recv (s, (void*)((char *)msg + start), len, flags);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_SSize_t) 
Socket_recvFrom (C_Sock_t s, Array(Word8_t) msg, 
                 C_Int_t start, C_Size_t len, C_Int_t flags,
                 Array(Word8_t) addr, Ref(C_Socklen_t) addrlen) {
  int out;
  
  MLton_initSockets ();
  out = MLton_recvfrom (s, (void*)((char *)msg + start), len, flags,
                        (struct sockaddr*)addr, (socklen_t*)addrlen);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

static inline C_Errno_t(C_SSize_t)
Socket_send (C_Sock_t s, Pointer msg, 
             C_Int_t start, C_Size_t len, C_Int_t flags) {
  int out;
  
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
  int out;
  
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
Socket_Ctl_getSockOpt (C_Sock_t s, C_Int_t level, C_Int_t optname, 
                       Array(Word8_t) optval, Ref(C_Socklen_t) optlen) {
  int out;
  
  MLton_initSockets ();
  out = getsockopt (s, level, optname, (void*)optval, (socklen_t*)optlen);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_Int_t)
Socket_Ctl_setSockOpt (C_Sock_t s, C_Int_t level, C_Int_t optname, 
                       Vector(Word8_t) optval, C_Socklen_t optlen) {
  int out;
  
  MLton_initSockets ();
  out = setsockopt (s, level, optname, (const void*)optval, (socklen_t)optlen);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_Int_t) 
Socket_Ctl_getIOCtl (C_Sock_t s, C_Int_t request, Array(Word8_t) argp) {
  int out;
  
  MLton_initSockets ();
  out = ioctl (s, request, (void*)argp);
  if (out == -1) MLton_fixSocketErrno ();
  
  return out;
}

C_Errno_t(C_Int_t) 
Socket_Ctl_setIOCtl (C_Sock_t s, C_Int_t request, Vector(Word8_t) argp) {
  int out;
  
  MLton_initSockets ();
  out = ioctl (s, request, (const void*)argp);
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
