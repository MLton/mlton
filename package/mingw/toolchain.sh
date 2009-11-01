#! /bin/bash

mirror="http://downloads.sourceforge.net/mingw-w64/files/"
mingw32="mingw-w32-1.0-bin_i686-mingw_20091023.zip"
mingw64="mingw-w64-1.0-bin_i686-mingw_20091023.zip"

for i in "$mingw32" "$mingw64"; do
  wget -c "$mirror/$i"
  mkdir tmp
  cd tmp
  7z x "../$i" 
  
  mv lib/libiberty.a *-mingw32/lib
  rm -rf mingw share man include info
  # toolchain binaries are 32-bit, but the libraries are 32/64 bit
  strip -p `find . -name \*.exe`
  chmod +x *-mingw32/bin/strip.exe # noop on mingw, but helps on linux
  ./*-mingw32/bin/strip.exe -p --strip-debug --strip-unneeded `find . -name \*.[ao]`
  
  dest=`basename "$i" .zip`
  7z a "../$dest.7z" -mx=9 *
  cd ..
  rm -rf tmp
done
