structure Sizes =
   struct
      type t =
          {char: {bits: int, align: int},
          short: {bits: int, align: int},
          int: {bits: int, align: int},
          long: {bits: int, align: int},
          longlong: {bits: int, align: int},
          float: {bits: int, align: int},
          double: {bits: int, align: int},
          longdouble: {bits: int, align: int},
          pointer: {bits: int, align: int},
          min_struct: {bits: int, align: int},
          min_union: {bits: int, align: int},
          onlyPackBitFields: bool,
          ignoreUnnamedBitFieldAlignment: bool}
   end