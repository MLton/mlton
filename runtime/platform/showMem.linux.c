void showMem () {
        static char buffer[256];

        sprintf (buffer, "/bin/cat /proc/%d/maps\n", (int)(getpid ()));
        system (buffer);
}
