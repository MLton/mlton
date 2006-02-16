#include <stdio.h>

typedef unsigned int uint;

typedef struct Cons {
        struct Cons *next;
        int value;
} *Cons;

Cons listCons (int n, Cons c) {
        Cons res;

        res = (Cons) malloc (sizeof(*res));
        fprintf (stderr, "0x%08x = listCons (%d)\n", (uint)res, n);
        res->next = c;
        res->value = n;
        return res;
}

Cons listSing (int n) {
        Cons res;

        res = (Cons) malloc (sizeof(*res));
        fprintf (stderr, "0x%08x = listSing (%d)\n", (uint)res, n);
        res->next = NULL;
        res->value = n;
        return res;
}

void listFree (Cons p) {
        fprintf (stderr, "listFree (0x%08x)\n", (uint)p);
        free (p);
}

int listSum (Cons c) {
        int res;

        fprintf (stderr, "listSum\n");
        res = 0;
        for (; c != NULL; c = c->next)
                res += c->value;
        return res;
}
