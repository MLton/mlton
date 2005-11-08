/* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "util.h"

#define prefixLines 26
static char* prefix[prefixLines] = {
  "/* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh",
  " *    Jagannathan, and Stephen Weeks.",
  " *",
  " * MLton is released under a BSD-style license.",
  " * See the file MLton-LICENSE for details.",
  " */",
  "",
  "/* Can't use _TYPES_H_ because MSVCRT uses it.  So, we use _MLTON_TYPES_H_. */",
  "",
  "#ifndef _MLTON_TYPES_H_",
  "#define _MLTON_TYPES_H_",
  "",
  "/* We need these because in header files for exported SML functions, types.h is",
  " * included without platform.h.",
  " */",
  "#ifndef _ISOC99_SOURCE",
  "#define _ISOC99_SOURCE",
  "#endif",
  "#if (defined (__OpenBSD__))",
  "#include <inttypes.h>",
  "#elif (defined (__sun__))",
  "#include <sys/int_types.h>",
  "#else",
  "#include <stdint.h>",
  "#endif",
  ""
};

#define stdtypesLines 21
static char* stdtypes[stdtypesLines] = {
  "typedef int8_t Int8;",
  "typedef int16_t Int16;",
  "typedef int32_t Int32;",
  "typedef int64_t Int64;",
  "typedef unsigned char* Pointer;",
  "typedef float Real32;",
  "typedef double Real64;",
  "typedef uint8_t Word8;",
  "typedef uint16_t Word16;",
  "typedef uint32_t Word32;",
  "typedef uint64_t Word64;",
  "",
  "typedef Int8 WordS8;",
  "typedef Int16 WordS16;",
  "typedef Int32 WordS32;",
  "typedef Int64 WordS64;",
  "",
  "typedef Word8 WordU8;",
  "typedef Word16 WordU16;",
  "typedef Word32 WordU32;",
  "typedef Word64 WordU64;"
};

#define systype(t, bt, name)               \
  do {                                     \
  writeString (fd, "typedef ");            \
  writeString (fd, bt);                    \
  writeUintmaxU (fd, CHAR_BIT * sizeof(t));\
  writeString (fd, " ");                   \
  writeString (fd, name);                  \
  writeString (fd, ";");                   \
  writeNewline (fd);                       \
  } while (0)

#define suffixLines 2
static char* suffix[suffixLines] = {
  "",
  "#endif /* _MLTON_TYPES_H_ */"
};

int main (int argc, char* argv[]) {
  int fd;

  fd = open_safe ("types.h", O_RDWR | O_CREAT, 0);
  for (int i = 0; i < prefixLines; i++) {
    writeString (fd, prefix[i]);
    writeNewline (fd);
  }
  for (int i = 0; i < stdtypesLines; i++) {
    writeString (fd, stdtypes[i]);
    writeNewline (fd);
  }
  writeNewline (fd);
  systype(char, "Word", "Char");
  systype(int, "Int", "Int");
  systype(off_t, "Int", "Position");
  systype(double, "Real", "Real");
  systype(unsigned int, "Word", "Word");
  systype(int, "Int", "Bool");
  writeNewline (fd);
  systype(unsigned char*, "Word", "Cpointer");
  systype(char*, "Word", "Cstring");
  systype(char**, "Word", "CstringArray");
  systype(size_t, "Int", "Size");
  systype(ssize_t, "Int", "Ssize");
  writeNewline (fd);
  systype(int, "Int", "Fd");
  systype(tcflag_t, "Word", "Flag");
  systype(gid_t, "Word", "Gid");
  systype(mode_t, "Int", "Mode");
  systype(pid_t, "Int", "Pid");
  systype(int, "Int", "Resource");
  systype(rlim_t, "Word", "Rlimit");
  systype(int, "Int", "Signal");
  systype(speed_t, "Word", "Speed");
  systype(int, "Int", "Status");
  systype(uid_t, "Word", "Uid");
  writeNewline (fd);
  for (int i = 0; i < suffixLines; i++) {
    writeString (fd, suffix[i]);
    writeNewline (fd);
  }
  return 0;
}
