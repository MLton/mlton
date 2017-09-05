Run before distributing:

```shell
autoreconf --install
./configure
make release
```

End user:
```shell
./configure --help
./configure --prefix=<PATH> --with-gmp=<PATH>
make
make install
make clean
```

TODO
===

- patch `mlton-script` based on exported WITH_GMP include and lib
- update `make release` to exclude autoconf-created files
- update build/install instructions in README
- output VERSION from `make version`
