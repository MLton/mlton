/* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */
#ifndef _NET_CONSTANTS_H_
#define _NET_CONSTANTS_H_

#include <stdlib.h>
#include <errno.h>
#if (defined __sun__)
#include <sys/filio.h> /* For FIONBIO, FIONREAD. */
#include <sys/sockio.h> /* For SIOCATMARK. */
#endif
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>

#if (defined __CYGWIN__)
#define MSG_DONTWAIT 0
#define PF_INET6 0
struct sockaddr_in6 {};
#endif

#define NetHostDB_inAddrLen sizeof(struct in_addr)
#define NetHostDB_INADDR_ANY INADDR_ANY
#define max(x,y) (((x) > (y)) ? (x) : (y))
#define Socket_sockAddrLenMax max(sizeof(struct sockaddr), \
			      max(sizeof(struct sockaddr_un), \
			      max(sizeof(struct sockaddr_in), \
				  sizeof(struct sockaddr_in6))))
#define Socket_AF_UNIX PF_UNIX
#define Socket_AF_INET PF_INET
#define Socket_AF_INET6 PF_INET6
#define Socket_AF_UNSPEC PF_UNSPEC
#define Socket_SOCK_STREAM SOCK_STREAM
#define Socket_SOCK_DGRAM SOCK_DGRAM
#define Socket_Ctl_SOL_SOCKET SOL_SOCKET
#define Socket_Ctl_SO_DEBUG SO_DEBUG
#define Socket_Ctl_SO_REUSEADDR SO_REUSEADDR
#define Socket_Ctl_SO_KEEPALIVE SO_KEEPALIVE
#define Socket_Ctl_SO_DONTROUTE SO_DONTROUTE
#define Socket_Ctl_SO_LINGER SO_LINGER
#define Socket_Ctl_SO_BROADCAST SO_BROADCAST
#define Socket_Ctl_SO_OOBINLINE SO_OOBINLINE
#define Socket_Ctl_SO_SNDBUF SO_SNDBUF
#define Socket_Ctl_SO_RCVBUF SO_RCVBUF
#define Socket_Ctl_SO_TYPE SO_TYPE
#define Socket_Ctl_SO_ERROR SO_ERROR
#define Socket_Ctl_FIONBIO FIONBIO
#define Socket_Ctl_FIONREAD FIONREAD
#define Socket_Ctl_SIOCATMARK SIOCATMARK
#define Socket_SHUT_RD SHUT_RD
#define Socket_SHUT_WR SHUT_WR
#define Socket_SHUT_RDWR SHUT_RDWR
#define Socket_MSG_DONTROUTE MSG_DONTROUTE
#define Socket_MSG_DONTWAIT MSG_DONTWAIT
#define Socket_MSG_OOB MSG_OOB
#define Socket_MSG_PEEK MSG_PEEK
#define Socket_INetSock_TCP_SOL_TCP IPPROTO_TCP
#define Socket_INetSock_TCP_SO_NODELAY TCP_NODELAY

#endif /* #ifndef _NET_CONSTANTS_H_ */
