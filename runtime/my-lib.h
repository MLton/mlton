#ifndef _MY_LIB_H
#define _MY_LIB_H

#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

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
#define	TRUE	(0 == 0)
#define	FALSE	(not TRUE)
#define	loop	while (TRUE)		/* loop until break */
#define	EOS	'\0'			/* end-of-string char */
#ifndef	NULL
#define	NULL	0			/* invalid pointer */
#endif

#define string char*

#define	unless(p)	if (not (p))
#define	until(p)	while (not (p))
#define	cardof(a)	(sizeof(a) / sizeof(*(a)))
#define	endof(a)	((a) + cardof(a))
#define	bitsof(a)	(sizeof(a) * 8)

/* issue error message and exit */
extern void	die(char *fmt, ...)
			__attribute__((format(printf, 1, 2)));

/* issue error message and exit.  Also print strerror(errno). */
extern void	diee(char *fmt, ...)
			__attribute__((format(printf, 1, 2)));

/* assertion failure routine */
extern void asfail(char *file, int line, char *prop);
/*
 * Assertion verifier.
 */
#if NODEBUG
#define	assert(p)	((void)0)
#else
#define	assert(p)	((p) ? (void)0 : asfail(__FILE__, __LINE__, #p))
#endif

FILE *sopen(char *fileName, char *mode);

/* safe version of fwrite */
void swrite(void *ptr, size_t size, size_t nmemb, FILE *file);
void swriteUint(uint n, FILE *file);

/* safe version of fread */
void sread(void *ptr, size_t size, size_t nmemb, FILE *file);
uint sreadUint(FILE *file);

/* safe mmap and munmap */
void *smmap(size_t length);
void smunmap(void *base, size_t length);

/* Return a statically allocated comma separated string */
string intToCommaString(int n);
string uintToCommaString(uint n);
string ullongToCommaString(ullong n);

#endif
