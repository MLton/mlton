Summary: An optimizing compiler for the Standard ML programming language.
Name: mlton
Version: VERSION
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
make bootstrap VERSION=%{version} 

%install
rm -rf $RPM_BUILD_ROOT
make install DESTDIR=$RPM_BUILD_ROOT VERSION=%{version}

%files
%attr(-, root, root)		/usr/share/doc/mlton
%attr(-, root, root)		/usr/bin/mllex
%attr(-, root, root)		/usr/bin/mlprof
%attr(-, root, root)		/usr/bin/mlton
%attr(-, root, root)		/usr/bin/mlyacc
%attr(-, root, root)		/usr/lib/mlton
%attr(-, root, root)		/usr/share/man/man1/mlprof.1.gz
%attr(-, root, root)		/usr/share/man/man1/mlton.1.gz

