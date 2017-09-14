#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])

m4_define([MLTON_VERSION], [`date +%Y%m%d.%H%M%S`])
AC_INIT([MLton], [MLTON_VERSION], [mlton-devel@mlton.org])

# if this file does not exist, `configure` was invoked in the wrong directory
AC_CONFIG_SRCDIR([bin/mlton-script.in])

########################################################################
# Set various environment variables (needed at configure time)
########################################################################
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

###############################################################################
# Check system C compiler and tools
###############################################################################

AC_PROG_CXX
AC_PROG_CC
AC_PROG_CC_STDC # enforce C99 standard whenever calling CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_CHECK_PROG(found_ar, ar, yes, no)
if test x$found_ar = xno
then
    AC_MSG_NOTICE([The 'ar' command must be in the path to build AC_PACKAGE_NAME])
    AC_MSG_NOTICE(['ar' is also part of the GNU 'binutils' package.])
    AC_MSG_ERROR([Exiting, as the archiver 'ar' can not be found.])
fi

AC_CHECK_PROG(found_ranlib, ranlib, yes, no)
if test x$found_ranlib = xno
then
    AC_MSG_NOTICE([The 'ranlib' command must be in the path to build AC_PACKAGE_NAME])
    AC_MSG_NOTICE(['ranlib' is also part of the GNU 'binutils' package.])
    AC_MSG_ERROR([Exiting, as 'ranlib' can not be found.])
fi
AC_PROG_RANLIB

AC_PROG_SED

# Checks for header files.
# Included for debugging only...
AC_CHECK_HEADERS([fcntl.h fenv.h float.h inttypes.h limits.h netdb.h netinet/in.h stddef.h stdint.h stdlib.h string.h strings.h sys/ioctl.h sys/param.h sys/socket.h sys/time.h sys/timeb.h syslog.h termios.h unistd.h utime.h])

# GMP is a required dependency, will fail at compile time if 
# not on C compiler's search path and --with-gmp-include  and
# --with-gmp-lib are not specified
AC_CHECK_HEADER([gmp.h],[],[
    AC_MSG_WARN([The 'gmp.h' header file is not on C compiler's search path.])
])

AC_CHECK_LIB([gmp],[__gmpz_sqrt],[],[
    AC_MSG_WARN([The 'libgmp' shared library is not on C compiler's search path.])
])


###############################################################################
# Check for MLton binary tools
###############################################################################
AC_CHECK_PROG(found_mlton, mlton, yes, no)
if test x$found_mlton = xno
then
    AC_MSG_WARN([You do not have 'mlton', which is recommended, but not])
    AC_MSG_WARN([required. However, some binary version of SML is required])
    AC_MSG_WARN([to compile from source. Refer to])
    AC_MSG_WARN([http://www.mlton.org/SelfCompiling for instructions.])
fi

AC_CHECK_PROG(found_mllex, mllex, yes, no)
AC_CHECK_PROG(found_ml_lex, ml-lex, yes, no)
if [ test x$found_mllex = xno ] && [ test x$found_ml_lex = xno ]
then
    AC_MSG_NOTICE([The SML scanner generator tool 'mllex'])
    AC_MSG_NOTICE([must be in the path to build AC_PACKAGE_NAME.])
    AC_MSG_NOTICE(['mllex' may also be named 'ml-lex'.])
    AC_MSG_ERROR([Exiting, as 'mllex' can not be found.])
fi

AC_CHECK_PROG(found_mlyacc, mlyacc, yes, no)
AC_CHECK_PROG(found_ml_yacc, ml-yacc, yes, no)
if [ test x$found_mlyacc = xno ] && [ test x$found_ml_yacc = xno ]
then
    AC_MSG_NOTICE([The SML parser generator tool 'mlyacc'])
    AC_MSG_NOTICE([must be in the path to build AC_PACKAGE_NAME.])
    AC_MSG_NOTICE(['mlyacc' may also be named 'ml-yacc'.])
    AC_MSG_ERROR([Exiting, as 'mlyacc' can not be found.])
fi

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
