#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])

m4_define([MLTON_VERSION], [`date +%Y%m%d-%H%M%S`])
AC_INIT([MLton], [MLTON_VERSION], [mlton-devel@mlton.org])

# if this file does not exist, `configure` was invoked in the wrong directory
AC_CONFIG_SRCDIR([bin/mlton-script])


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


dnl We don't really use automake, but configure needs aclocal and the
dnl automake library files.
AM_INIT_AUTOMAKE([1.9.6 foreign])

AC_CONFIG_FILES([Makefile-auto
                 Makefile])
AC_CONFIG_FILES([mlton-env-config])

AC_OUTPUT()

