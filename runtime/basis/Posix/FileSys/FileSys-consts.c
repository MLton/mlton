#include "platform.h"

const C_Int_t Posix_FileSys_A_F_OK = F_OK;
const C_Int_t Posix_FileSys_A_R_OK = R_OK;
const C_Int_t Posix_FileSys_A_W_OK = W_OK;
const C_Int_t Posix_FileSys_A_X_OK = X_OK;

const C_Int_t Posix_FileSys_O_RDONLY = O_RDONLY;
const C_Int_t Posix_FileSys_O_RDWR = O_RDWR;
const C_Int_t Posix_FileSys_O_WRONLY = O_WRONLY;

const C_Int_t Posix_FileSys_O_APPEND = O_APPEND;
#ifndef O_BINARY
#define O_BINARY 0
#endif
const C_Int_t Posix_FileSys_O_BINARY = O_BINARY;
const C_Int_t Posix_FileSys_O_CREAT = O_CREAT;
#ifndef O_DSYNC
#define O_DSYNC 0
#endif
const C_Int_t Posix_FileSys_O_DSYNC = O_DSYNC;
const C_Int_t Posix_FileSys_O_EXCL = O_EXCL;
const C_Int_t Posix_FileSys_O_NOCTTY = O_NOCTTY;
const C_Int_t Posix_FileSys_O_NONBLOCK = O_NONBLOCK;
#ifndef O_RSYNC
#define O_RSYNC 0
#endif
const C_Int_t Posix_FileSys_O_RSYNC = O_RSYNC;
const C_Int_t Posix_FileSys_O_SYNC = O_SYNC;
#ifndef O_TEXT
#define O_TEXT 0
#endif
const C_Int_t Posix_FileSys_O_TEXT = O_TEXT;
const C_Int_t Posix_FileSys_O_TRUNC = O_TRUNC;

#ifndef _PC_FILESIZEBITS
#define _PC_FILESIZEBITS -1
#endif
const C_Int_t Posix_FileSys_PC_FILESIZEBITS = _PC_FILESIZEBITS;
#ifndef _PC_LINK_MAX
#define _PC_LINK_MAX -1
#endif
#ifndef _PC_LINK_MAX
#define _PC_LINK_MAX -1
#endif
const C_Int_t Posix_FileSys_PC_LINK_MAX = _PC_LINK_MAX;
#ifndef _PC_MAX_CANON
#define _PC_MAX_CANON -1
#endif
const C_Int_t Posix_FileSys_PC_MAX_CANON = _PC_MAX_CANON;
#ifndef _PC_MAX_INPUT
#define _PC_MAX_INPUT -1
#endif
const C_Int_t Posix_FileSys_PC_MAX_INPUT = _PC_MAX_INPUT;
#ifndef _PC_NAME_MAX
#define _PC_NAME_MAX -1
#endif
const C_Int_t Posix_FileSys_PC_NAME_MAX = _PC_NAME_MAX;
#ifndef _PC_PATH_MAX
#define _PC_PATH_MAX -1
#endif
const C_Int_t Posix_FileSys_PC_PATH_MAX = _PC_PATH_MAX;
#ifndef _PC_PIPE_BUF
#define _PC_PIPE_BUF -1
#endif
const C_Int_t Posix_FileSys_PC_PIPE_BUF = _PC_PIPE_BUF;
#ifndef _PC_2_SYMLINKS
#define _PC_2_SYMLINKS -1
#endif
const C_Int_t Posix_FileSys_PC_TWO_SYMLINKS = _PC_2_SYMLINKS;
#ifndef _PC_ALLOC_SIZE_MIN
#define _PC_ALLOC_SIZE_MIN -1
#endif
const C_Int_t Posix_FileSys_PC_ALLOC_SIZE_MIN = _PC_ALLOC_SIZE_MIN;
#ifndef _PC_REC_INCR_XFER_SIZE
#define _PC_REC_INCR_XFER_SIZE -1
#endif
const C_Int_t Posix_FileSys_PC_REC_INCR_XFER_SIZE = _PC_REC_INCR_XFER_SIZE;
#ifndef _PC_REC_MAX_XFER_SIZE
#define _PC_REC_MAX_XFER_SIZE -1
#endif
const C_Int_t Posix_FileSys_PC_REC_MAX_XFER_SIZE = _PC_REC_MAX_XFER_SIZE;
#ifndef _PC_REC_MIN_XFER_SIZE
#define _PC_REC_MIN_XFER_SIZE -1
#endif
const C_Int_t Posix_FileSys_PC_REC_MIN_XFER_SIZE = _PC_REC_MIN_XFER_SIZE;
#ifndef _PC_REC_XFER_ALIGN
#define _PC_REC_XFER_ALIGN -1
#endif
const C_Int_t Posix_FileSys_PC_REC_XFER_ALIGN = _PC_REC_XFER_ALIGN;
#ifndef _PC_SYMLINK_MAX
#define _PC_SYMLINK_MAX -1
#endif
const C_Int_t Posix_FileSys_PC_SYMLINK_MAX = _PC_SYMLINK_MAX;
#ifndef _PC_CHOWN_RESTRICTED
#define _PC_CHOWN_RESTRICTED -1
#endif
const C_Int_t Posix_FileSys_PC_CHOWN_RESTRICTED = _PC_CHOWN_RESTRICTED;
#ifndef _PC_NO_TRUNC
#define _PC_NO_TRUNC -1
#endif
const C_Int_t Posix_FileSys_PC_NO_TRUNC = _PC_NO_TRUNC;
#ifndef _PC_VDISABLE
#define _PC_VDISABLE -1
#endif
const C_Int_t Posix_FileSys_PC_VDISABLE = _PC_VDISABLE;
#ifndef _PC_ASYNC_IO
#define _PC_ASYNC_IO -1
#endif
const C_Int_t Posix_FileSys_PC_ASYNC_IO = _PC_ASYNC_IO;
#ifndef _PC_PRIO_IO
#define _PC_PRIO_IO -1
#endif
const C_Int_t Posix_FileSys_PC_PRIO_IO = _PC_PRIO_IO;
#ifndef _PC_SYNC_IO
#define _PC_SYNC_IO -1
#endif
const C_Int_t Posix_FileSys_PC_SYNC_IO = _PC_SYNC_IO;

const C_Mode_t Posix_FileSys_S_IFBLK = S_IFBLK;
const C_Mode_t Posix_FileSys_S_IFCHR = S_IFCHR;
const C_Mode_t Posix_FileSys_S_IFDIR = S_IFDIR;
const C_Mode_t Posix_FileSys_S_IFIFO = S_IFIFO;
const C_Mode_t Posix_FileSys_S_IFLNK = S_IFLNK;
const C_Mode_t Posix_FileSys_S_IFMT = S_IFMT;
const C_Mode_t Posix_FileSys_S_IFREG = S_IFREG;
const C_Mode_t Posix_FileSys_S_IFSOCK = S_IFSOCK;
const C_Mode_t Posix_FileSys_S_IRGRP = S_IRGRP;
const C_Mode_t Posix_FileSys_S_IROTH = S_IROTH;
const C_Mode_t Posix_FileSys_S_IRUSR = S_IRUSR;
const C_Mode_t Posix_FileSys_S_IRWXG = S_IRWXG;
const C_Mode_t Posix_FileSys_S_IRWXO = S_IRWXO;
const C_Mode_t Posix_FileSys_S_IRWXU = S_IRWXU;
const C_Mode_t Posix_FileSys_S_ISGID = S_ISGID;
const C_Mode_t Posix_FileSys_S_ISUID = S_ISUID;
const C_Mode_t Posix_FileSys_S_ISVTX = S_ISVTX;
const C_Mode_t Posix_FileSys_S_IWGRP = S_IWGRP;
const C_Mode_t Posix_FileSys_S_IWOTH = S_IWOTH;
const C_Mode_t Posix_FileSys_S_IWUSR = S_IWUSR;
const C_Mode_t Posix_FileSys_S_IXGRP = S_IXGRP;
const C_Mode_t Posix_FileSys_S_IXOTH = S_IXOTH;
const C_Mode_t Posix_FileSys_S_IXUSR = S_IXUSR;
