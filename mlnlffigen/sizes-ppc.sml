(* This file was automatically generated using size.c.
 * It contains information about c data sizes and layout.

 * Limitations:
 *   1. write proper test for bitFieldAlignment.
 *   2. include date and system information in this file?
 *)

structure SizesPPC = struct
val sizes = {    (*** all sizes in bits ***)
  char = {bits = 8, align = 8},
  short = {bits = 16, align = 16},
  int = {bits = 32, align = 32},
  long = {bits = 32, align = 32},
  longlong = {bits = 64, align = 32},
  float = {bits = 32, align = 32},
  double = {bits = 64, align = 32},
  longdouble = {bits = 64, align = 32},
  pointer = {bits = 32, align = 32},
  min_struct = {bits = 8, align = 8},
  min_union = {bits = 8, align = 8},
  onlyPackBitFields = false,
  ignoreUnnamedBitFieldAlignment = true
}
end
