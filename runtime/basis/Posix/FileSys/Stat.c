#include "platform.h"

static struct stat statbuf;

C_Dev_t Posix_FileSys_Stat_getDev (void) {
  return statbuf.st_dev;
}

C_INo_t Posix_FileSys_Stat_getINo (void) {
  return statbuf.st_ino;
}

C_Mode_t Posix_FileSys_Stat_getMode (void) {
  return statbuf.st_mode;
}

C_NLink_t Posix_FileSys_Stat_getNLink (void) {
  return statbuf.st_nlink;
}

C_UId_t Posix_FileSys_Stat_getUId (void) {
  return statbuf.st_uid;
}

C_GId_t Posix_FileSys_Stat_getGId (void) {
  return statbuf.st_gid;
}

C_Dev_t Posix_FileSys_Stat_getRDev (void) {
  return statbuf.st_rdev;
}

C_Off_t Posix_FileSys_Stat_getSize (void) {
  return statbuf.st_size;
}

C_Time_t Posix_FileSys_Stat_getATime (void) {
  return statbuf.st_atime;
}

C_Time_t Posix_FileSys_Stat_getMTime (void) {
  return statbuf.st_mtime;
}

C_Time_t Posix_FileSys_Stat_getCTime (void) {
  return statbuf.st_ctime;
}

/*
C_BlkSize_t Posix_FileSys_Stat_getBlkSize (void) {
  return statbuf.st_blksize;
}

C_BlkCnt_t Posix_FileSys_Stat_getBlkCnt (void) {
  return statbuf.st_blocks;
}
*/

C_Errno_t(C_Int_t) Posix_FileSys_Stat_fstat (C_Fd_t f) {
  return fstat (f, &statbuf);
}

C_Errno_t(C_Int_t) Posix_FileSys_Stat_lstat (NullString8_t f) {
  return lstat ((const char*)f, &statbuf);
}

C_Errno_t(C_Int_t) Posix_FileSys_Stat_stat (NullString8_t f) {
  return stat ((const char*)f, &statbuf);
}
