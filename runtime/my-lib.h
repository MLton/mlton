#ifndef _MY_LIB_H
#define _MY_LIB_H

#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#ifndef ASSERT
#define ASSERT 0
#endif

/*
 * The following definitions make C more amenable to a purist.
 */
#define	bool	int			/* boolean type */
#define	uint	unsigned int		/* short names for unsigned types */
#define	ulong	unsigned long
#define	ullong	unsigned long long	/* GCC extension */
#define	llong	long long		/* GCC extension */
#define	uchar	unsigned char
#define	ushort	unsigned short int
#define	not	!			/* logical negation operator */
#define	and	&&			/* logical conjunction */
#define	or	||			/* logical disjunction */
#ifndef TRUE
#define	TRUE	(0 == 0)
#endif
#ifndef FALSE
#define	FALSE	(not TRUE)
#endif
#define	loop	while (TRUE)		/* loop until break */
#define	EOS	'\0'			/* end-of-string char */
#ifndef	NULL
#define	NULL	0			/* invalid pointer */
#endif

#define NEW(x) \
	x = (typeof(x))smalloc(sizeof(*x))
#define ARRAY(a, s) \
	a = (typeof(a))scalloc(s, sizeof(*a))

#define string char*

#define	unless(p)	if (not (p))
#define	until(p)	while (not (p))
#define	cardof(a)	(sizeof(a) / sizeof(*(a)))
#define	endof(a)	((a) + cardof(a))
#define	bitsof(a)	(sizeof(a) * 8)

/* issue error message and exit */
extern void	die(char *fmt, ...)
			__attribute__((format(printf, 1, 2)))
			__attribute__ ((noreturn));

/* issue error message and exit.  Also print strerror(errno). */
extern void	diee(char *fmt, ...)
			__attribute__((format(printf, 1, 2)))
			__attribute__ ((noreturn));

/* assertion failure routine */
extern void asfail(char *file, int line, char *prop);
/*
 * Assertion verifier.
 */
#if ASSERT
#define	assert(p)	((p) ? (void)0 : asfail(__FILE__, __LINE__, #p))
#else
#define	assert(p)	((void)0)
#endif

string boolToString (bool b);

void *scalloc (size_t nmemb, size_t size);

/* safe version of close, mkstemp, write */
int smkstemp (char *template);
void sclose (int fd);
void swrite (int fd, const void *buf, size_t count);
void swriteUint (int fd, uint n);
void swriteUllong (int fd, ullong n);

/* safe versions of fopen, fread, fwrite */
void sfclose (FILE *file);
FILE *sfopen (char *fileName, char *mode);
void sfread (void *ptr, size_t size, size_t nmemb, FILE *file);
uint sfreadUint (FILE *file);
void sfwrite (void *ptr, size_t size, size_t nmemb, FILE *file);
void sfwriteUint (uint n, FILE *file);

void *smalloc (size_t length);

/* safe mmap and munmap */
void *smmap (size_t length);
void smunmap (void *base, size_t length);
void sunlink (char *path);

/* Return a statically allocated comma separated string */
string intToCommaString (int n);
string uintToCommaString (uint n);
string ullongToCommaString (ullong n);

#endif
