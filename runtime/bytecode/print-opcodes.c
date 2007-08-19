/* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "platform.h"
#include <stdio.h>
#include "opcode.h"

int main (__attribute__((unused)) int argc, 
          __attribute__((unused)) char* argv[]) {
        Opcode opc;
        unsigned int i;

        unless (cardof (opcodeStrings) < (1 << (8 * sizeof (opc))))
                die ("too many opcodes\n");
        for (i = 0; i < cardof (opcodeStrings); ++i)
                fprintf (stdout, "%s\n", opcodeStrings[i]);
        return 0;
}
