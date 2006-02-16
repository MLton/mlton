#include "platform.h"

enum {
        INDENTATION = 1,
};

static int depth = 0;

static void spaces(int depth) {
        int i;

        depth %= 40;
        for (i = 0; i < depth; ++i)
                fprintf(stderr, " ");
}

void Debug_enter(Pointer name) {
        depth += INDENTATION;
        spaces(depth);
        fprintf(stderr, "Entering ");
        Stdio_print(name);
        fprintf(stderr, "\n");
}

void Debug_leave(Pointer name) {
        spaces(depth);
        fprintf(stderr, "Leaving ");
        Stdio_print(name);
        fprintf(stderr, "\n");
        depth -= INDENTATION;
}
