#include "my-lib.h"

#include <errno.h>
#include <string.h>
#include <sys/mman.h>
#if (defined (__CYGWIN__))
#include <windows.h>
#endif
#if (defined (__linux__))
#include <values.h>
#endif
#if (defined (__FreeBSD__))
#include <limits.h>
#endif

void
die(char *fmt, ...)
{
	va_list	args;

	fflush(stdout);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	exit(1);
}

void diee (char * fmt, ...)
{
	va_list	args;

	fflush(stdout);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	
	fprintf(stderr, " (%s)\n", strerror(errno));
	exit(1);
}

void asfail(char *file, int line, char *prop)
{
	fflush(stdout);
	fprintf(stderr, "%s %d: assert(%s) failed.\n", file, line, prop);
	abort();
}

void sclose (int fd) {
	unless (0 == close (fd)) 
		diee ("unable to close %d", fd);
}

int smkstemp (char *template) {
	int fd;

	fd = mkstemp (template);
	if (-1 == fd)
		diee ("unable to make temporary file");
	return fd;
}

/* safe version of write */
void swrite(int fd, const void *buf, size_t count) {
	if (0 == count) return;
	unless (count == write(fd, buf, count))
		diee("swrite failed");
}

void swriteUint(int fd, uint n) {
	swrite(fd, &n, sizeof(uint));
}

/* safe version of fclose */
void sfclose (FILE *file) {
	unless (0 == fclose (file))
		diee ("unable to close file");
}

/* safe version of fopen */
FILE *sfopen(char *fileName, char *mode) {
	FILE *file;
	
	if (NULL == (file = fopen((char*)fileName, mode)))
		diee("sfopen unable to open file %s", fileName);
	return file;
}

/* safe version of fwrite */
void sfwrite(void *ptr, size_t size, size_t nmemb, FILE *file) {
	size_t bytes;

	bytes = size * nmemb;
	if (0 == bytes) return;
	unless (1 == fwrite(ptr, size * nmemb, 1, file))
		diee("sfwrite failed");
}

void sfwriteUint(uint n, FILE *file) {
	sfwrite(&n, sizeof(uint), 1, file);
}

void sfread(void *ptr, size_t size, size_t nmemb, FILE *file) {
	size_t bytes;

	bytes = size * nmemb;
	if (0 == bytes) return;
	unless (1 == fread(ptr, bytes, 1, file))
		diee("sfread failed");
}

uint sfreadUint(FILE *file) {
	uint n;

	sfread(&n, sizeof(uint), 1, file);

	return n;
}

/* ------------------------------------------------- */
/*                 intToCommaString                  */
/* ------------------------------------------------- */
/* BUF_SIZE must be == 1 mod 4 so that the ',' is inserted in the right place */
#define BUF_SIZE 81

string intToCommaString(int n) {
	static char buf[BUF_SIZE];
	int i;
	
	i = BUF_SIZE - 1;
	buf[i--] = '\000';
	
	if (0 == n)
		buf[i--] = '0';
#if (defined (__CYGWIN__) || defined (__FreeBSD__))
#define MININT 0x80000000
#endif
 	else if (MININT == n) {
		/* must treat MININT specially, because I negate stuff later */
		strcpy(buf + 1, "-2,147,483,648");
		i = 0;
	} else {
		int m;
	
 		if (n > 0) m = n; else m = -n;
	
		while (m > 0) {
 			buf[i--] = m % 10 + '0';
			m = m / 10;
 			if (i % 4 == 0 and m > 0) buf[i--] = ',';
 		}
 		if (n < 0) buf[i--] = '-';
 	}
 	return buf + i + 1;
}

string uintToCommaString(uint n) {
	static char buf[BUF_SIZE];
	int i;
	
	i = BUF_SIZE - 1;
	buf[i--] = '\000';
	
	if (0 == n)
		buf[i--] = '0';
        else {
		while (n > 0) {
 			buf[i--] = n % 10 + '0';
			n = n / 10;
 			if (i % 4 == 0 and n > 0) buf[i--] = ',';
 		}
 	}
 	return buf + i + 1;
}

string ullongToCommaString(ullong n) {
	static char buf[BUF_SIZE];
	int i;
	
	i = BUF_SIZE - 1;
	buf[i--] = '\000';
	
	if (0 == n)
		buf[i--] = '0';
        else {
		while (n > 0) {
 			buf[i--] = n % 10 + '0';
			n = n / 10;
 			if (i % 4 == 0 and n > 0) buf[i--] = ',';
 		}
 	}
 	return buf + i + 1;
}

/* ------------------------------------------------- */
/*               Safe mmap and munmap                */
/* ------------------------------------------------- */

void *smmap(size_t length) {
	void *result;

#if (defined (__CYGWIN__))	
	result = VirtualAlloc (0, length, MEM_COMMIT, PAGE_READWRITE);
	if (NULL == result)
		die("VirtualAlloc failed");
#elif (defined (__linux__) || defined (__FreeBSD__))
	result = mmap(NULL, length, PROT_READ | PROT_WRITE, 
			MAP_PRIVATE | MAP_ANON, -1, 0);
	if (result == (void*)-1) 
		diee("Out of swap space.");
#endif	
	return result;
}

void smunmap(void *base, size_t length) {
	assert(base != NULL);
	if (0 == length)
		return;
	if (0 != munmap(base, length))
		diee("munmap failed");
}

void sunlink (char *path) {
	unless (0 == unlink (path))
		diee ("unkink (%s) failed", path);
}
