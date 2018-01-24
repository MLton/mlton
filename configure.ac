#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])

# caveat: MLTON_VERSION might differ between the configuration and build 
#         steps if autoreconf is not re-run at every build
m4_define([MLTON_VERSION], m4_esyscmd([TZ=UTC git log -n1 --date=format-local:"%Y%m%d.%H%M%S" --pretty=format:"%cd-g%h$( [ "$(git status --porcelain 2> /dev/null)" ] && echo '-dirty')" 2> /dev/null || echo '????????']))
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

AC_PROG_CC
AC_PROG_CC_STDC
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

# GMP is a required dependency
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
    AC_MSG_WARN([You don't seem to have 'mlton'.])
    AC_MSG_WARN([A prior install of MLton is recommended, though not required.])
    AC_MSG_WARN([However, some binary version of SML is required])
    AC_MSG_WARN([to compile from source.])
    AC_MSG_WARN([Refer to http://www.mlton.org/SelfCompiling for instructions.])
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

AC_CONFIG_FILES([Makefile
                 benchmark/Makefile
                 bin/mlton-script
                 lib/mlnlffi-lib/Makefile
                 mlton/Makefile
                 mlyacc/Makefile
                 package/freebsd/Makefile
                 package/mingw/Makefile
                 runtime/Makefile])

AC_OUTPUT
