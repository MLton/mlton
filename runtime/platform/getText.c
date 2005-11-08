/* To get the beginning and end of the text segment. */
extern char _start;
extern char etext;

void *GC_getTextStart () {
        return &_start;
}
void *GC_getTextEnd () {
        return &etext;
}
