#include "platform.h"

static struct stat statbuf;

Word Posix_FileSys_Stat_dev () {
        return statbuf.st_dev;
}

Int Posix_FileSys_Stat_ino () {
        return statbuf.st_ino;
}

Word Posix_FileSys_Stat_mode () {
        return statbuf.st_mode;
}

Int Posix_FileSys_Stat_nlink () {
        return statbuf.st_nlink;
}

Word Posix_FileSys_Stat_uid () {
        return statbuf.st_uid;
}

Word Posix_FileSys_Stat_gid () {
        return statbuf.st_gid;
}

Word Posix_FileSys_Stat_rdev () {
        return statbuf.st_rdev;
}

Position Posix_FileSys_Stat_size () {
        return statbuf.st_size;
}

Int Posix_FileSys_Stat_atime () {
        return statbuf.st_atime;
}

Int Posix_FileSys_Stat_mtime () {
        return statbuf.st_mtime;
}

Int Posix_FileSys_Stat_ctime () {
        return statbuf.st_ctime;
}

Int Posix_FileSys_Stat_fstat (Fd f) {
        return fstat (f, &statbuf);
}

Int Posix_FileSys_Stat_lstat (NullString f) {
        return lstat ((char*)f, &statbuf);
}

Int Posix_FileSys_Stat_stat (NullString f) {        
        return stat ((char*)f, &statbuf);
}
