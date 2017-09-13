#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])

m4_define([MLTON_VERSION], [`date +%Y%m%d.%H%M%S`])
AC_INIT([MLton], [MLTON_VERSION], [mlton-devel@mlton.org])

# if this file does not exist, `configure` was invoked in the wrong directory
AC_CONFIG_SRCDIR([bin/mlton-script.in])

AC_ARG_WITH([gmp-include],
            [AS_HELP_STRING([--with-gmp-include=PATH],
                            [specify directory for the installed GMP include file (gmp.h)])],
            [with_gmp_include="$withval"],
            [with_gmp_include='']
           )
AC_SUBST(WITH_GMP_INC_DIR, $with_gmp_include)

AC_ARG_WITH([gmp-lib],
            [AS_HELP_STRING([--with-gmp-lib=PATH],
                            [specify directory for the installed GMP library (libgmp.so or libgmp.a or libgmp.dylib)])],
            [with_gmp_lib="$withval"],
            [with_gmp_lib='']
           )
AC_SUBST(WITH_GMP_LIB_DIR, $with_gmp_lib)

AC_ARG_VAR([XCFLAGS],[extra C compiler flags])
AC_ARG_VAR([XLDFLAGS],[extra linker flags])

AC_CONFIG_HEADERS([config.h])



# FIXME: working below

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CC_STDC # enforce C99 standard whenever calling CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AC_PROG_SED

# Checks for libraries.
# FIXME: Replace `main' with a function in `-ldl':
AC_CHECK_LIB([dl], [main])
# FIXME: Replace `main' with a function in `-lmlton':
AC_CHECK_LIB([mlton], [main])
# FIXME: Replace `main' with a function in `-local_icons':
AC_CHECK_LIB([ocal_icons], [main])
# FIXME: Replace `main' with a function in `-lpolymain':
AC_CHECK_LIB([polymain], [main])
# FIXME: Replace `main' with a function in `-lpolyml':
AC_CHECK_LIB([polyml], [main])


# Checks for header files.
AC_CHECK_HEADERS([fcntl.h fenv.h float.h inttypes.h limits.h netdb.h netinet/in.h stddef.h stdint.h stdlib.h string.h strings.h sys/ioctl.h sys/param.h sys/socket.h sys/time.h sys/timeb.h syslog.h termios.h unistd.h utime.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_TYPE_UID_T
AC_C_INLINE
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_CHECK_MEMBERS([struct stat.st_rdev])
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_CHOWN
AC_FUNC_FORK
AC_FUNC_GETGROUPS
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_FUNC_MKTIME
AC_FUNC_MMAP
AC_FUNC_STRTOD
AC_CHECK_FUNCS([alarm atexit dup2 fchdir fesetround floor ftruncate getcwd gethostbyaddr gethostbyname gethostname gettimeofday memmove memset mkdir mkfifo modf munmap pathconf putenv rint rmdir select setenv socket sqrt strdup strerror sysinfo tzset uname utime])

dnl We don't really use automake, but configure needs aclocal and the
dnl automake library files.
AM_INIT_AUTOMAKE([1.9.6 foreign])

# AC_CONFIG_FILES([Makefile-auto
#                  Makefile
#                  runtime/Makefile
#                  bin/mlton-script
#                 ])

## FIXME: merge AC_CONFIG_FILES
AC_CONFIG_FILES([Makefile-auto
                 Makefile
                 basis-library/Makefile
                 basis-library/libs/basis-2002/top-level/Makefile
                 benchmark/Makefile
                 benchmark/tests/Makefile
                 bin/Makefile
                 bin/mlton-script
                 doc/examples/Makefile
                 doc/examples/ffi/Makefile
                 doc/examples/finalizable/Makefile
                 doc/examples/profiling/Makefile
                 doc/examples/save-world/Makefile
                 doc/guide/Makefile
                 doc/hacker-guide/Makefile
                 doc/library-guide/Makefile
                 doc/mlb-formal/Makefile
                 doc/style-guide/Makefile
                 include/Makefile
                 lib/Makefile
                 lib/ckit-lib/Makefile
                 lib/mllpt-lib/Makefile
                 lib/mlnlffi-lib/Makefile
                 lib/mlrisc-lib/Makefile
                 lib/mlton/Makefile
                 lib/mlton/basic/Makefile
                 lib/smlnj-lib/Makefile
                 lib/stubs/Makefile
                 lib/stubs/basis-stubs-for-smlnj/Makefile
                 lib/stubs/mlton-stubs-for-smlnj/Makefile
                 lib/stubs/mlton-stubs/Makefile
                 man/Makefile
                 mllex/Makefile
                 mlnlffigen/Makefile
                 mlprof/Makefile
                 mlton/Makefile
                 mlton/front-end/Makefile
                 mlyacc/Makefile
                 mlyacc/doc/Makefile
                 package/freebsd/Makefile
                 package/macosx/Makefile
                 package/mingw/Makefile
                 regression/Makefile
                 regression/library/Makefile
                 runtime/Makefile
                 util/cm2mlb/Makefile])


AC_OUTPUT

