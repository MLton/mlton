/* To get the beginning and end of the text segment. */
#if defined(PIC)
extern unsigned char _init;
#else
extern unsigned char _start;
#endif
extern unsigned char etext;

code_pointer GC_getTextStart (void) {
#if defined(PIC)
        return &_init;
#else
        return &_start;
#endif
}

code_pointer GC_getTextEnd (void) {
        return &etext;
}
