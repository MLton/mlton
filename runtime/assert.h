#ifndef ASSERT
#define ASSERT 0
#endif

/* assertion failure routine */
extern void asfail (char *file, int line, char *prop);
/*
 * Assertion verifier.
 */
#if ASSERT
#define	assert(p) ((p) ? (void)0 : asfail(__FILE__, __LINE__, #p))
#else
#define	assert(p) ((void)0)
#endif
