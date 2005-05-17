/* To get the beginning and end of the text segment. */
extern char _start;
extern char etext;

void *getTextStart () {
	return &_start;
}
void *getTextEnd () {
	return &etext;
}
