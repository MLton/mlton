/* To get the beginning and end of the text segment. */
extern void _start(void);
extern void etext(void);

void *getTextStart () {
	return &_start;
}
void *getTextEnd () {
	return &etext;
}
