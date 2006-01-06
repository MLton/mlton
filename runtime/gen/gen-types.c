/* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "cenv.h"
#include "util.h"

static char* prefix[] = {
  "/* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh",
  " *    Jagannathan, and Stephen Weeks.",
  " *",
  " * MLton is released under a BSD-style license.",
  " * See the file MLton-LICENSE for details.",
  " */",
  "",
  "/* Can't use _TYPES_H_ because MSVCRT uses it.",
  " * So, we use _MLTON_TYPES_H_.",
  " */",
  "",
  "#ifndef _MLTON_TYPES_H_",
  "#define _MLTON_TYPES_H_",
  "",
  "/* We need these because in header files for exported SML functions, ",
  " * types.h is included without cenv.h.",
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
  "",
  NULL
};

static char* stdtypes[] = {
  "/* ML types */",
  "typedef unsigned char* Pointer;",
  "#define Array(t) Pointer",
  "#define Ref(t) Pointer",
  "#define Vector(t) Pointer",
  "",
  "typedef int8_t Int8;",
  "typedef int16_t Int16;",
  "typedef int32_t Int32;",
  "typedef int64_t Int64;",
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
  "typedef Word64 WordU64;",
  "",
  "typedef WordS8 Char8;",
  "typedef WordS16 Char16;",
  "typedef WordS32 Char32;",
  "",
  "typedef Vector(Char8) String8;",
  "typedef Vector(Char16) String16;",
  "typedef Vector(Char32) String32;",
  "",
  "typedef Int32 Bool;",
  "typedef Char8 Char;",
  "typedef Int32 Int;",
  "typedef Real64 Real;",
  "typedef String8 String;",
  "typedef Word32 Word;",
  ""
  "typedef String NullString;",
  NULL
};

#define systype(t, bt, name)               \
  do {                                     \
  writeString (fd, "typedef ");            \
  writeString (fd, "/* ");                 \
  writeString (fd, #t);                    \
  writeString (fd, " */ ");                \
  writeString (fd, bt);                    \
  writeUintmaxU (fd, CHAR_BIT * sizeof(t));\
  writeString (fd, " ");                   \
  writeString (fd, name);                  \
  writeString (fd, ";");                   \
  writeNewline (fd);                       \
  } while (0)
#define chkintsystype(t, name)             \
  do {                                     \
  if ((double)((t)(-1)) > 0)               \
  systype(t, "Word", name);                \
  else                                     \
  systype(t, "Int", name);                 \
  } while (0)
#define chknumsystype(t, name)             \
  do {                                     \
  if ((double)((t)(0.25)) > 0)             \
  systype(t, "Real", name);                \
  else                                     \
  chkintsystype(t, name);                  \
  } while (0)

static char* suffix[] = {
  "",
  "typedef C_Pointer Cpointer;",
  "typedef C_Size Size;",
  "typedef C_SSize Ssize;",
  "typedef C_String Cstring;",
  "typedef C_StringArray CstringArray;",
  "typedef C_Off Position;"
  "",
  "typedef C_Fd Fd;",
  "typedef C_TCFlag Flag;",
  "typedef C_GId Gid;",
  "typedef C_Mode Mode;",
  "typedef C_PId Pid;",
  "typedef C_Resource Resource;",
  "typedef C_RLim Rlimit;",
  "typedef C_Signal Signal;",
  "typedef C_Speed Speed;",
  "typedef C_Status Status;",
  "typedef C_UId Uid;",
  "",
  "#endif /* _MLTON_TYPES_H_ */",
  NULL
};

int main (int argc, char* argv[]) {
  int fd;

  fd = open_safe ("types.h", O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
  for (int i = 0; prefix[i] != NULL; i++) {
    writeString (fd, prefix[i]);
    writeNewline (fd);
  }
  for (int i = 0; stdtypes[i] != NULL; i++) {
    writeString (fd, stdtypes[i]);
    writeNewline (fd);
  }
  writeNewline (fd);
  writeString (fd, "/* C */");
  writeNewline (fd);
  chkintsystype(char, "C_Char");
  systype(signed char, "Int", "C_SChar");
  systype(unsigned char, "Word", "C_UChar");
  systype(short, "Int", "C_Short");
  systype(unsigned short, "Word", "C_UShort");
  systype(int, "Int", "C_Int");
  systype(unsigned int, "Word", "C_UInt");
  systype(long, "Int", "C_Long");
  systype(unsigned long, "Word", "C_ULong");
  systype(long long, "Int", "C_LongLong");
  systype(unsigned long long, "Word", "C_ULongLong");
  systype(float, "Real", "C_Float");
  systype(double, "Real", "C_Double");
  // systype(long double, "Real", "C_LongDouble");
  systype(size_t, "Word", "C_Size");
  writeNewline (fd);
  systype(void*, "Word", "C_Pointer");
  systype(char*, "Word", "C_String");
  systype(char**, "Word", "C_StringArray");
  writeNewline (fd);
  writeString (fd, "/* C99 */");
  writeNewline (fd);
  systype(intmax_t, "Int", "C_Intmax");
  systype(uintmax_t, "Word", "C_Uintmax");
  systype(intptr_t, "Int", "C_Intptr");
  systype(uintptr_t, "Word", "C_UIntptr");
  writeNewline (fd);
  writeString (fd, "/* from <sys/resource.h> */");
  writeNewline (fd);
  systype(rlim_t, "Word", "C_RLim");
  writeNewline (fd);
  writeString (fd, "/* from <sys/types.h> */");
  writeNewline (fd);
  chknumsystype(clock_t, "C_Clock");
  chkintsystype(gid_t, "C_GId");
  chkintsystype(id_t, "C_Id");
  chkintsystype(mode_t, "C_Mode");
  systype(off_t, "Int", "C_Off");
  systype(pid_t, "Int", "C_PId");
  systype(ssize_t, "Int", "C_SSize");
  systype(suseconds_t, "Int", "C_SUSeconds");
  chknumsystype(time_t, "C_Time");
  chkintsystype(uid_t, "C_UId");
  systype(useconds_t, "Word", "C_USeconds");
  writeNewline (fd);
  writeString (fd, "/* from <termios.h> */");
  writeNewline (fd);
  systype(tcflag_t, "Word", "C_TCFlag");
  systype(speed_t, "Word", "C_Speed");
  writeNewline (fd);
  writeString (fd, "/* Generic integers */");
  writeNewline (fd);
  systype(int, "Int", "C_Fd");
  systype(int, "Int", "C_Resource");
  systype(int, "Int", "C_Signal");
  systype(int, "Int", "C_Status");
  writeNewline (fd);
  for (int i = 0; suffix[i] != NULL; i++) {
    writeString (fd, suffix[i]);
    writeNewline (fd);
  }
  return 0;
}
