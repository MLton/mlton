Summary: An optimizing compiler for the Standard ML programming language.
Name: mlton
Version:
Release:
Copyright: GPL
Group: Development/Languages
Source: mlton-%{version}.tgz
URL: http://www.mlton.org/
Buildroot: %{_tmppath}/%{name}/mlton
Prefix: /usr

%description
MLton is a whole-program optimizing compiler for the Standard ML programming
language.  The MLton home page is http://www.mlton.org/.

%prep
%setup

%build
make STUBS=no

%install
rm -rf $RPM_BUILD_ROOT
make prefix=$RPM_BUILD_ROOT VERSION=%{version} install

%files
%attr(-, root, root)		/usr/share/doc/mlton-%{version}
%attr(-, root, root)		/usr/local/bin/mllex
%attr(-, root, root)		/usr/local/bin/mlprof
%attr(-, root, root)		/usr/local/bin/mlton
%attr(-, root, root)		/usr/local/bin/mlyacc
%attr(-, root, root)		/usr/local/lib/mlton
%attr(-, root, root)		/usr/local/man/man1/mlprof.1
%attr(-, root, root)		/usr/local/man/man1/mlton.1

