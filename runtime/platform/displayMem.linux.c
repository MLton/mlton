void GC_displayMem (void) {
        static char buffer[256];
        int res;

        snprintf (buffer, cardof(buffer), "/bin/cat /proc/%d/maps\n", (int)(getpid ()));
        res = system (buffer);
        if (-1 == res)
          return;
}
