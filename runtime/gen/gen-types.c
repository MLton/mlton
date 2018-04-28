/* Copyright (C) 2012,2017 Matthew Fluet.
 * Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#include "cenv.h"
#include "util.h"

static const char* mlTypesHPrefix[] = {
  "/* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh",
  " *    Jagannathan, and Stephen Weeks.",
  " *",
  " * MLton is released under a HPND-style license.",
  " * See the file MLton-LICENSE for details.",
  " */",
  "",
  "#ifndef _MLTON_MLTYPES_H_",
  "#define _MLTON_MLTYPES_H_",
  "",
  "/* We need these because in header files for exported SML functions, ",
  " * types.h is included without cenv.h.",
  " */",
  "#if (defined (_AIX) || defined (__hpux__) || defined (__OpenBSD__))",
  "#include <inttypes.h>",
  "#elif (defined (__sun__))",
  "#include <sys/int_types.h>",
  "#else",
  "#include <stdint.h>",
  "#endif",
  "",
  NULL
};

static const char* cTypesHPrefix[] = {
  "/* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh",
  " *    Jagannathan, and Stephen Weeks.",
  " *",
  " * MLton is released under a HPND-style license.",
  " * See the file MLton-LICENSE for details.",
  " */",
  "",
  "#ifndef _MLTON_CTYPES_H_",
  "#define _MLTON_CTYPES_H_",
  "",
  NULL
};

static const char* cTypesSMLPrefix[] = {
  "(* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh",
  " *    Jagannathan, and Stephen Weeks.",
  " *",
  " * MLton is released under a HPND-style license.",
  " * See the file MLton-LICENSE for details.",
  " *)",
  "",
  NULL
};

static const char* mlTypesHStd[] = {
  "/* ML types */",
  // "typedef void* Pointer;",
  // "typedef uintptr_t Pointer;",
  // "typedef unsigned char* Pointer;",
  "typedef unsigned char PointerAux __attribute__ ((may_alias));",
  "typedef PointerAux* Pointer;",
  "#define Array(t) Pointer",
  "#define Ref(t) Pointer",
  "#define Vector(t) Pointer",
  "",
  "typedef int8_t Int8_t;",
  "typedef int8_t Int8;",
  "typedef int16_t Int16_t;",
  "typedef int16_t Int16;",
  "typedef int32_t Int32_t;",
  "typedef int32_t Int32;",
  "typedef int64_t Int64_t;",
  "typedef int64_t Int64;",
  "typedef float Real32_t;",
  "typedef float Real32;",
  "typedef double Real64_t;",
  "typedef double Real64;",
  // "typedef long double Real128_t;",
  // "typedef long double Real128;",
  "typedef uint8_t Word8_t;",
  "typedef uint8_t Word8;",
  "typedef uint16_t Word16_t;",
  "typedef uint16_t Word16;",
  "typedef uint32_t Word32_t;",
  "typedef uint32_t Word32;",
  "typedef uint64_t Word64_t;",
  "typedef uint64_t Word64;",
  "",
  "typedef Int8_t WordS8_t;",
  "typedef Int8_t WordS8;",
  "typedef Int16_t WordS16_t;",
  "typedef Int16_t WordS16;",
  "typedef Int32_t WordS32_t;",
  "typedef Int32_t WordS32;",
  "typedef Int64_t WordS64_t;",
  "typedef Int64_t WordS64;",
  "",
  "typedef Word8_t WordU8_t;",
  "typedef Word8_t WordU8;",
  "typedef Word16_t WordU16_t;",
  "typedef Word16_t WordU16;",
  "typedef Word32_t WordU32_t;",
  "typedef Word32_t WordU32;",
  "typedef Word64_t WordU64_t;",
  "typedef Word64_t WordU64;",
  "",
  "typedef WordU8_t Char8_t;",
  "typedef WordU8_t Char8;",
  "typedef WordU16_t Char16_t;",
  "typedef WordU16_t Char16;",
  "typedef WordU32_t Char32_t;",
  "typedef WordU32_t Char32;",
  "",
  "typedef Vector(Char8_t) String8_t;",
  "typedef Vector(Char8_t) String8;",
  "typedef Vector(Char16_t) String16_t;",
  "typedef Vector(Char16_t) String16;",
  "typedef Vector(Char32_t) String32_t;",
  "typedef Vector(Char32_t) String32;",
  "",
  "typedef Int32_t Bool_t;",
  "typedef Int32_t Bool;",
  // "typedef Char8_t Char_t;",
  // "typedef Char8_t Char;",
  // "typedef Int32_t Int_t;",
  // "typedef Int32_t Int;",
  // "typedef Real64_t Real_t;",
  // "typedef Real64_t Real;",
  // "typedef String8_t String_t;",
  // "typedef String8_t String;",
  // "typedef Word32_t Word_t;",
  // "typedef Word32_t Word;",
  ""
  "typedef String8_t NullString8_t;",
  "typedef String8_t NullString8;",
  "",
  "typedef void* CPointer;",
  "typedef Pointer Objptr;",
  NULL
};

#define booltype(t, bt, name)                       \
  do {                                              \
  writeString (cTypesHFd, "typedef");               \
  writeString (cTypesHFd, " /* ");                  \
  writeString (cTypesHFd, #t);                      \
  writeString (cTypesHFd, " */ ");                  \
  writeString (cTypesHFd, bt);                      \
  writeUintmaxU (cTypesHFd, CHAR_BIT * sizeof(t));  \
  writeString (cTypesHFd, "_t");                    \
  writeString (cTypesHFd, " ");                     \
  writeString (cTypesHFd, "C_");                    \
  writeString (cTypesHFd, name);                    \
  writeString (cTypesHFd, "_t;");                   \
  writeNewline (cTypesHFd);                         \
  writeString (cTypesSMLFd, "structure C_");        \
  writeString (cTypesSMLFd, name);                  \
  writeString (cTypesSMLFd, " = WordToBool (");     \
  writeString (cTypesSMLFd, "type t = ");           \
  writeString (cTypesSMLFd, "Word");                \
  writeUintmaxU (cTypesSMLFd, CHAR_BIT * sizeof(t));\
  writeString (cTypesSMLFd, ".word");               \
  writeString (cTypesSMLFd, " ");                   \
  writeString (cTypesSMLFd, "val zero: t = 0wx0");  \
  writeString (cTypesSMLFd, " ");                   \
  writeString (cTypesSMLFd, "val one: t = 0wx1");   \
  writeString (cTypesSMLFd, ")");                   \
  writeNewline (cTypesSMLFd);                       \
  } while (0)
#define systype(t, bt, name)                        \
  do {                                              \
  char *btLower = strdup(bt);                       \
  for (size_t i = 0; i < strlen(btLower); i++)      \
    btLower[i] = (char)(tolower((int)(bt[i])));     \
  char *btUpper = strdup(bt);                       \
  for (size_t i = 0; i < strlen(btUpper); i++)      \
    btUpper[i] = (char)(toupper((int)(bt[i])));     \
  writeString (cTypesHFd, "typedef");               \
  writeString (cTypesHFd, " /* ");                  \
  writeString (cTypesHFd, #t);                      \
  writeString (cTypesHFd, " */ ");                  \
  writeString (cTypesHFd, bt);                      \
  writeUintmaxU (cTypesHFd, CHAR_BIT * sizeof(t));  \
  writeString (cTypesHFd, "_t");                    \
  writeString (cTypesHFd, " ");                     \
  writeString (cTypesHFd, "C_");                    \
  writeString (cTypesHFd, name);                    \
  writeString (cTypesHFd, "_t;");                   \
  writeNewline (cTypesHFd);                         \
  writeString (cTypesSMLFd, "structure C_");        \
  writeString (cTypesSMLFd, name);                  \
  writeString (cTypesSMLFd, " = struct open ");     \
  writeString (cTypesSMLFd, bt);                    \
  writeUintmaxU (cTypesSMLFd, CHAR_BIT * sizeof(t));\
  writeString (cTypesSMLFd, " type t = ");          \
  writeString (cTypesSMLFd, btLower);               \
  writeString (cTypesSMLFd, " end");                \
  writeNewline (cTypesSMLFd);                       \
  writeString (cTypesSMLFd, "functor C_");          \
  writeString (cTypesSMLFd, name);                  \
  writeString (cTypesSMLFd, "_Choose");             \
  writeString (cTypesSMLFd, bt);                    \
  writeString (cTypesSMLFd, "N (A: CHOOSE_");       \
  writeString (cTypesSMLFd, btUpper);               \
  writeString (cTypesSMLFd, "N_ARG) = Choose");     \
  writeString (cTypesSMLFd, bt);                    \
  writeString (cTypesSMLFd, "N_");                  \
  writeString (cTypesSMLFd, bt);                    \
  writeUintmaxU (cTypesSMLFd, CHAR_BIT * sizeof(t));\
  writeString (cTypesSMLFd, " (A)");                \
  writeNewline (cTypesSMLFd);                       \
  free (btLower);                                   \
  free (btUpper);                                   \
  } while (0)
#define chksystype(t, name)                \
  do {                                     \
  if ((double)((t)(0.25)) > 0)             \
  systype(t, "Real", name);                \
  else if ((double)((t)(-1)) > 0)          \
  systype(t, "Word", name);                \
  else                                     \
  systype(t, "Int", name);                 \
  } while (0)
#define ptrtype(t, name)                            \
  do {                                              \
  systype(t, "Word", name);                         \
  } while (0)

#define aliastype(name1, bt, name2)                 \
  do {                                              \
  char *btLower = strdup(bt);                       \
  for (size_t i = 0; i < strlen(btLower); i++)      \
    btLower[i] = (char)(tolower((int)(bt[i])));     \
  char *btUpper = strdup(bt);                       \
  for (size_t i = 0; i < strlen(btUpper); i++)      \
    btUpper[i] = (char)(toupper((int)(bt[i])));     \
  writeString (cTypesHFd, "typedef ");              \
  writeString (cTypesHFd, "C_");                    \
  writeString (cTypesHFd, name1);                   \
  writeString (cTypesHFd, "_t ");                   \
  writeString (cTypesHFd, "C_");                    \
  writeString (cTypesHFd, name2);                   \
  writeString (cTypesHFd, "_t;");                   \
  writeNewline (cTypesHFd);                         \
  writeString (cTypesSMLFd, "structure C_");        \
  writeString (cTypesSMLFd, name2);                 \
  writeString (cTypesSMLFd, " = C_");               \
  writeString (cTypesSMLFd, name1);                 \
  writeNewline (cTypesSMLFd);                       \
  writeString (cTypesSMLFd, "functor C_");          \
  writeString (cTypesSMLFd, name2);                 \
  writeString (cTypesSMLFd, "_Choose");             \
  writeString (cTypesSMLFd, bt);                    \
  writeString (cTypesSMLFd, "N (A: CHOOSE_");       \
  writeString (cTypesSMLFd, btUpper);               \
  writeString (cTypesSMLFd, "N_ARG) = C_");         \
  writeString (cTypesSMLFd, name1);                 \
  writeString (cTypesSMLFd, "_Choose");             \
  writeString (cTypesSMLFd, bt);                    \
  writeString (cTypesSMLFd, "N (A)");               \
  writeNewline (cTypesSMLFd);                       \
  free (btLower);                                   \
  free (btUpper);                                   \
  } while (0)

static const char* mlTypesHSuffix[] = {
  "#endif /* _MLTON_MLTYPES_H_ */",
  NULL
};

static const char* cTypesHSuffix[] = {
  "#define C_Errno_t(t) t",
  "",
  "#endif /* _MLTON_CTYPES_H_ */",
  NULL
};

static const char* cTypesSMLSuffix[] = {
  NULL
};

int main (__attribute__ ((unused)) int argc,
          __attribute__ ((unused)) char* argv[]) {
  FILE *mlTypesHFd;
  FILE *cTypesHFd;
  FILE *cTypesSMLFd;

  mlTypesHFd = fopen_safe ("ml-types.h", "w");
  for (int i = 0; mlTypesHPrefix[i] != NULL; i++)
    writeStringWithNewline (mlTypesHFd, mlTypesHPrefix[i]);
  for (int i = 0; mlTypesHStd[i] != NULL; i++)
    writeStringWithNewline (mlTypesHFd, mlTypesHStd[i]);
  for (int i = 0; mlTypesHSuffix[i] != NULL; i++)
    writeStringWithNewline (mlTypesHFd, mlTypesHSuffix[i]);

  cTypesHFd = fopen_safe ("c-types.h", "w");
  cTypesSMLFd = fopen_safe ("c-types.sml", "w");

  for (int i = 0; cTypesHPrefix[i] != NULL; i++)
    writeStringWithNewline (cTypesHFd, cTypesHPrefix[i]);
  for (int i = 0; cTypesSMLPrefix[i] != NULL; i++)
    writeStringWithNewline (cTypesSMLFd, cTypesSMLPrefix[i]);

  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  writeStringWithNewline (cTypesHFd, "/* C */");
  writeStringWithNewline (cTypesSMLFd, "(* C *)");
  booltype(_Bool, "Word", "Bool");
  chksystype(char, "Char");
  chksystype(signed char, "SChar");
  chksystype(unsigned char, "UChar");
  chksystype(short, "Short");
  chksystype(signed short, "SShort");
  chksystype(unsigned short, "UShort");
  chksystype(int, "Int");
  chksystype(signed int, "SInt");
  chksystype(unsigned int, "UInt");
  chksystype(long, "Long");
  chksystype(signed long, "SLong");
  chksystype(unsigned long, "ULong");
  chksystype(long long, "LongLong");
  chksystype(signed long long, "SLongLong");
  chksystype(unsigned long long, "ULongLong");
  chksystype(float, "Float");
  chksystype(double, "Double");
  // chksystype(long double, "LongDouble");
  chksystype(size_t, "Size");
  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  ptrtype(unsigned char*, "Pointer");
  // ptrtype(void*, "Pointer");
  // ptrtype(uintptr_t, "Pointer");
  ptrtype(char*, "String");
  ptrtype(char**, "StringArray");

  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  writeStringWithNewline (cTypesHFd, "/* Generic integers */");
  writeStringWithNewline (cTypesSMLFd, "(* Generic integers *)");
  aliastype("Int", "Int", "Fd");
  aliastype("Int", "Int", "Signal");
  aliastype("Int", "Int", "Status");
  aliastype("Int", "Int", "Sock");

  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  writeStringWithNewline (cTypesHFd, "/* C99 */");
  writeStringWithNewline (cTypesSMLFd, "(* C99 *)");
  chksystype(ptrdiff_t, "Ptrdiff");
  chksystype(intmax_t, "Intmax");
  chksystype(uintmax_t, "UIntmax");
  chksystype(intptr_t, "Intptr");
  chksystype(uintptr_t, "UIntptr");

  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  writeStringWithNewline (cTypesHFd, "/* from <dirent.h> */");
  writeStringWithNewline (cTypesSMLFd, "(* from <dirent.h> *)");
  // ptrtype(DIR*, "DirP");
  systype(DIR*, "Word", "DirP");

  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  writeStringWithNewline (cTypesHFd, "/* from <poll.h> */");
  writeStringWithNewline (cTypesSMLFd, "(* from <poll.h> *)");
  chksystype(nfds_t, "NFds");

  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  writeStringWithNewline (cTypesHFd, "/* from <resource.h> */");
  writeStringWithNewline (cTypesSMLFd, "(* from <resource.h> *)");
  chksystype(rlim_t, "RLim");

  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  writeStringWithNewline (cTypesHFd, "/* from <sys/types.h> */");
  writeStringWithNewline (cTypesSMLFd, "(* from <sys/types.h> *)");
  // chksystype(blkcnt_t, "BlkCnt");
  // chksystype(blksize_t, "BlkSize");
  chksystype(clock_t, "Clock");
  chksystype(dev_t, "Dev");
  chksystype(gid_t, "GId");
  // chksystype(id_t, "Id");
  chksystype(ino_t, "INo");
  chksystype(mode_t, "Mode");
  chksystype(nlink_t, "NLink");
  chksystype(off_t, "Off");
  chksystype(pid_t, "PId");
  chksystype(ssize_t, "SSize");
  chksystype(suseconds_t, "SUSeconds");
  chksystype(time_t, "Time");
  chksystype(uid_t, "UId");
  // chksystype(useconds_t, "USeconds");

  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  writeStringWithNewline (cTypesHFd, "/* from <sys/socket.h> */");
  writeStringWithNewline (cTypesSMLFd, "(* from <sys/socket.h> *)");
  chksystype(socklen_t, "Socklen");

  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  writeStringWithNewline (cTypesHFd, "/* from <termios.h> */");
  writeStringWithNewline (cTypesSMLFd, "(* from <termios.h> *)");
  chksystype(cc_t, "CC");
  chksystype(speed_t, "Speed");
  chksystype(tcflag_t, "TCFlag");

  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  writeStringWithNewline (cTypesHFd, "/* from \"gmp.h\" */");
  writeStringWithNewline (cTypesSMLFd, "(* from \"gmp.h\" *)");
  chksystype(mp_limb_t, "MPLimb");

  writeNewline (cTypesHFd);writeNewline (cTypesSMLFd);
  for (int i = 0; cTypesHSuffix[i] != NULL; i++)
    writeStringWithNewline (cTypesHFd, cTypesHSuffix[i]);
  for (int i = 0; cTypesSMLSuffix[i] != NULL; i++)
    writeStringWithNewline (cTypesSMLFd, cTypesSMLSuffix[i]);

  fclose_safe(mlTypesHFd);
  fclose_safe(cTypesHFd);
  fclose_safe(cTypesSMLFd);

  return 0;
}
