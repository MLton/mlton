# MLton

> MLton is a whole-program optimizing compiler for the Standard ML
> programming language.

## Features

  + Portability. Runs on the following platforms:

    - ARM: Linux (Debian).
    - Alpha: Linux (Debian).
    - AMD64: Darwin (Mac OS X), FreeBSD, Linux (Debian, Fedora, ...),
        Solaris (10 and above).
    - HPPA: HPUX (11.11 and above), Linux (Debian).
    - IA64: HPUX (11.11 and above), Linux (Debian).
    - PowerPC: AIX (5.2 and above), Darwin (Mac OS X), Linux (Debian,
        Fedora).
    - PowerPC64: AIX (5.2 and above).
    - S390: Linux (Debian).
    - Sparc: Linux (Debian), Solaris (8 and above).
    - X86: Cygwin/Windows, Darwin (Mac OS X), FreeBSD, Linux (Debian,
        Fedora, ...), MinGW/Windows, NetBSD, OpenBSD, Solaris (10 and
        above).

  + Robustness.

    - Supports the full SML 97 language as given in The Definition 
        of Standard ML (Revised).
    - A complete implementation of the Basis Library.
    - Generates standalone executables.
    - Compiles large programs.
    - Support for large amounts of memory (up to 4G on 32-bit systems; 
        more on 64-bit systems).
    - Support for large array lengths (up to 2^31 - 1 on 32-bit
        systems; up to 2^63-1 on 64-bit systems).
    - Support for large files, using 64-bit file positions.

  + Performance.

    - Executables have excellent running times.
    - Generates small executables.
    - Untagged and unboxed native integers, reals, and words.
    - Unboxed native arrays.
    - Multiple garbage collection strategies.
    - Fast arbitrary-precision arithmetic based on the GnuMP.

  + Tools.

    - Source-level profiling for both time and allocation.
    - MLLex lexer generator.
    - MLYacc parser generator.
    - ML-NLFFIGEN foreign-function-interface generator.

  + Extensions.

    - A simple and fast C FFI that supports calling from SML to C and 
        from C to SML.
    - The ML Basis system for programming in the very large.
    - Libraries for continuations, finalization, interval timers,
        random numbers, resource limits, resource usage, signal
        handlers, object size, system logging, threads, weak pointers,
        and world save and restore.

## Instructions to build from source

### Prerequisites

 - GCC 4.1.x or later
 - GNU Multiple Precision Library. Check if GMP is already present
     on your system (`whereis gmp libgmp`), typically on GCC search path
     (`gcc -v -x c -E -`).
 - MLton (recommended) or SML/NJ bootstrapping compiler. 
     The easiest way is to install a pre-built binary package for MLton 
     using your package manager. If using SML/NJ, see
     [Compiling with SML/NJ](http://mlton.org/SelfCompiling).

### Build instructions

MLton can be built from source using the usual `./configure && make && make install` 
sequence to install under the system default installation directory
(typically, `/usr/local`). Optionally, you can specify the `--prefix`
argument to install MLton into a custom directory.

You can instruct MLton to look for `gmp.h` outside your include
path or for `libgmp` outside your library search path using
`--with-gmp-lib` and `--with-gmp-include` configure options, as in:

```shell
gzip -cd mlton-*.tgz | tar xvf -

# execute under the extracted directory
./configure \
  --prefix=$HOME/opt \
  --with-gmp-include=/usr/local/include \
  --with-gmp-lib=/usr/local/lib
make
make install
```

Optionally, you can pass C compiler and linker flags to set
MLton's `-cc-opt` and `-link-opt`
[compile-time options](http://www.mlton.org/CompileTimeOptions)
via `XCFLAGS` and `XLDFLAGS` environment variables:
```shell
./configure \
  XCFLAGS='-I/usr/local/include' \
  XLDFLAGS='-L/usr/local/lib'
```

If building from source cloned from Github, first generate the `configure`
script (requires `GNU Autotools`):
```shell
autoreconf -vfi
# can now ./configure && make && make check && make install && make clean
```

To uninstall, simply delete the directory containing the MLton installation.


## Resources

For more information, go to the MLton home page. 

        http://mlton.org/

There are two mailing lists available.

 * MLton-devel@mlton.org  MLton developers
 * MLton-user@mlton.org   MLton user community 

`doc` (and `share/doc`) directory contents:

        README                  this file
        changelog               changelog
        cm2mlb/                 a utility for producing ML Basis programs in SML/NJ
        cmcat/                  a utility for producing whole programs in SML/NJ
        examples/               example SML programs
        guide/                  HTML MLton guide (copy of the MLton wiki)
        license/                license information
        mllex.pdf               user guide for mllex lexer generator
        mlton-guide.pdf         PDF version of MLton guide
        mlyacc.pdf              user guide for mlyacc parser generator


## Need help? Found a bug?

[Submit an issue](https://github.com/MLton/mlton/issues)
if you need any help. We welcome pull requests with bug fixes or changes.
