#include <sys/types.h>
#include <utime.h>
#include "mlton-posix.h"

static struct utimbuf utimbuf;

void Posix_FileSys_Utimbuf_setActime(Int i) {
	utimbuf.actime = i;
}

void Posix_FileSys_Utimbuf_setModTime(Int i) {
	utimbuf.modtime = i;
} 

Int Posix_FileSys_Utimbuf_utime(NullString s) {
	return (Int)utime((char *)s, &utimbuf);
}
