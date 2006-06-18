void GC_displayMem (void) {
        static char buffer[256];

        snprintf (buffer, cardof(buffer), "/bin/cat /proc/%d/maps\n", (int)(getpid ()));
        system (buffer);
}
