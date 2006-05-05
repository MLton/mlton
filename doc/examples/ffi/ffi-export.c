#include <stdio.h>
#include "export.h"

void g () {
        Char8 c;

        fprintf (stderr, "g starting\n");
        c = f (13, 17.15, 'a');
        fprintf (stderr, "g done  char = %c\n", c);
}

Pointer g2 () {
        Pointer res;
        fprintf (stderr, "g2 starting\n");
        res = f2 (0xFF);
        fprintf (stderr, "g2 done\n");
        return res;
}

void g3 () {
        fprintf (stderr, "g3 starting\n");
        f3 ();
        fprintf (stderr, "g3 done\n");
}

void g4 (Int32 i) {
        fprintf (stderr, "g4 (%d)\n", i);
        f4 (i);
}

void g5 () {
        fprintf (stderr, "g5 ()\n");
        fprintf (stderr, "zzz = %i\n", zzz);
        fprintf (stderr, "g5 done\n");
}
