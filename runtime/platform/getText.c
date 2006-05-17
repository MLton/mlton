/* To get the beginning and end of the text segment. */
extern unsigned char _start;
extern unsigned char etext;

code_pointer GC_getTextStart (void) {
        return &_start;
}

code_pointer GC_getTextEnd (void) {
        return &etext;
}
