#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

static struct utimbuf utimbuf;

void Posix_FileSys_Utimbuf_setActime (Int i) {
	utimbuf.actime = i;
}

void Posix_FileSys_Utimbuf_setModTime (Int i) {
	utimbuf.modtime = i;
} 

Int Posix_FileSys_Utimbuf_utime (NullString s) {
	return (Int)utime((char *)s, &utimbuf);
}
