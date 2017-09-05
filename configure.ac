#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])

m4_define(MLTON_VERSION, m4_normalize(m4_include(VERSION)))
# software name, version, where to report bugs
AC_INIT([MLton], [MLTON_VERSION], [mlton-devel@mlton.org])

# if this file does not exist, `configure` was invoked in the wrong directory
AC_CONFIG_SRCDIR([VERSION])


dnl TODO: work on custom GMP path
# gmplibs="-lmpc"
# gmpinc=
# have_gmp=no
AC_ARG_WITH([gmp],
            [AS_HELP_STRING([--with-gmp=PATH],
                            [specify prefix directory for the installed GMP package.])],
            [with_gmp="$withval"],
            [with_gmp='']
            )
AC_SUBST(WITH_GMP, $with_gmp)
# if test "x$with_gmp" != x; then
#   gmplibs="-L$with_gmp/lib $gmplibs"
#   gmpinc="-I$with_gmp/include $gmpinc"
#   have_gmp=yes
# fi

dnl We don't really use automake, but configure needs aclocal and the
dnl automake library files.
AM_INIT_AUTOMAKE([1.9.6 foreign])

AC_CONFIG_FILES([Makefile-auto
                 Makefile])
AC_CONFIG_FILES([mlton-env-config])

AC_OUTPUT()

