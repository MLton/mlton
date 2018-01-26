#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])

# caveat: MLTON_VERSION might differ between the configuration and build 
#         steps if autoreconf is not re-run at every build
m4_define([MLTON_VERSION], m4_esyscmd([TZ=UTC git log -n1 --date=format-local:"%Y%m%d.%H%M%S" --pretty=format:"%cd-g%h$( [ "$(git status --porcelain 2> /dev/null)" ] && echo '-dirty')" 2> /dev/null || echo '????????']))
AC_INIT([MLton], [MLTON_VERSION], [mlton-devel@mlton.org], [], [http://mlton.org])

# if this file does not exist, `configure` was invoked in the wrong directory
AC_CONFIG_SRCDIR([bin/mlton-script.in])

########################################################################
# Set various environment variables (needed at configure time)
########################################################################

AC_CONFIG_HEADERS([config.h])

###############################################################################
# Check system C compiler and tools
###############################################################################

AC_PROG_CC
AC_PROG_CC_STDC
AN_MAKEVAR([AR], [AC_PROG_AR])
AN_PROGRAM([ar], [AC_PROG_AR])
AC_DEFUN([AC_PROG_AR],[
  AC_CHECK_TOOL(AR, ar)
  test -z "$AR" && AC_MSG_ERROR([no acceptable archiver found in \$PATH])
])
AC_PROG_AR
AC_PROG_RANLIB

AC_PROG_MAKE_SET

AC_PROG_SED

###############################################################################
# Check for GMP
###############################################################################

AC_ARG_WITH(
  [gmp-include],
  [AS_HELP_STRING(
    [--with-gmp-include=PATH],
    [specify directory for the installed GMP header (gmp.h)])],
  [
  with_gmp_include="$withval"
  CPPFLAGS="$CPPFLAGS -I$withval"
  ],
  [with_gmp_include='']
)
AC_SUBST(WITH_GMP_INC_DIR, $with_gmp_include)

AC_ARG_WITH(
  [gmp-lib],
  [AS_HELP_STRING(
    [--with-gmp-lib=PATH],
    [specify directory for the installed GMP library (libgmp.so or libgmp.a or libgmp.dylib)])],
  [
  with_gmp_lib="$withval"
  LDFLAGS="$LDFLAGS -L$withval"
  ],
  [with_gmp_lib='']
)
AC_SUBST(WITH_GMP_LIB_DIR, $with_gmp_lib)

AC_CHECK_HEADER(
  [gmp.h],[],
  [AC_MSG_ERROR([The 'gmp.h' header file is not on C compiler's search path.])],
  [/**/]
)
AC_CHECK_LIB(
  [gmp],[__gmpz_sqrt],[],
  [AC_MSG_ERROR([The 'libgmp' shared library is not on C compiler's search path.])]
)

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
if [ test x$found_mllex = xno ]
then
AC_CHECK_PROG(found_ml_lex, ml-lex, yes, no)
if [ test x$found_ml_lex = xno ]
then
    AC_MSG_ERROR([No SML scanner generator tool ('mllex' or 'ml-lex') found in \$PATH])
fi
fi

AC_CHECK_PROG(found_mlyacc, mlyacc, yes, no)
if [ test x$found_mlyacc = xno ]
then
AC_CHECK_PROG(found_ml_yacc, ml-yacc, yes, no)
if [ test x$found_ml_yacc = xno ]
then
    AC_MSG_ERROR([No SML parser generator tool ('mlyacc' or 'ml-yacc') found in \$PATH])
fi
fi

AC_CONFIG_FILES([Makefile
                 benchmark/Makefile
                 bin/mlton-script
                 lib/mlnlffi-lib/Makefile
                 mlton/Makefile
                 mlyacc/Makefile
                 runtime/Makefile])

AC_OUTPUT
