Summary: An optimizing compiler for the Standard ML programming language.
Name: mlton
Version:
Release:
Copyright: GPL
Group: Development/Languages
Source: mlton-%{version}.tgz
Buildroot: %{_tmppath}/%{name}/mlton

%description
MLton is a whole-program optimizing compiler for the Standard ML programming
language.  The MLton home page is http://www.sourcelight.com/MLton.

%prep
%setup

%build
cd src
make

%install
cd src
rm -rf $RPM_BUILD_ROOT
make PREFIX=$RPM_BUILD_ROOT install

%files
%attr(-, root, root)	%doc	doc/*
%attr(-, root, root)		/usr/local/bin/mllex
%attr(-, root, root)		/usr/local/bin/mlprof
%attr(-, root, root)		/usr/local/bin/mlton
%attr(-, root, root)		/usr/local/bin/mlyacc
%attr(-, root, root)		/usr/local/lib/mlton
%attr(-, root, root)		/usr/local/man/man1/mlprof.1
%attr(-, root, root)		/usr/local/man/man1/mlton.1

