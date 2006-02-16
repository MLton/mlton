The following regressions are known to fail when using a version of
MLton built by SML/NJ.  This happens because of bugs in SML/NJ.

   pack-real.sml
        fails because SML/NJ has the wrong sign for 
        Real.fromString "~0.0".

----------------------------------------------------------------------

Many of these regression tests come from the ML Kit Version 3
distribution, which borrowed them from the Moscow ML distribution.

The "regression" script runs all the tests in this directory and
prints whether they succeeded or failed.  If they fail, you should
look at the "log" file to see what went wrong.  There should be only
two warnings in the log file.

        filesys.sml
                warning due to the use of tmpnam
        real.sml 
                fails due to MLton's incorrect handling of real to string
                conversions.   

The following subdirectories contain tests that have not yet been integrated
into the regression script.

fail/
        contains tests that should fail to compile.

modules/
        contains tests of the module system.

nonterminate/
        contains tests that should compile, but when run, should not terminate.

