
MLTON_CODEGEN_STATIC_INLINE
Pointer CPointer_add (Pointer p, C_Size_t s);
MLTON_CODEGEN_STATIC_INLINE
C_Size_t CPointer_diff (Pointer p1, Pointer p2);
MLTON_CODEGEN_STATIC_INLINE
Bool CPointer_equal (Pointer p1, Pointer p2);
MLTON_CODEGEN_STATIC_INLINE
Pointer CPointer_fromWord (C_Pointer_t x);
MLTON_CODEGEN_STATIC_INLINE
Bool CPointer_lt (Pointer p1, Pointer p2);
MLTON_CODEGEN_STATIC_INLINE
Pointer CPointer_sub (Pointer p, C_Size_t s);
MLTON_CODEGEN_STATIC_INLINE
C_Pointer_t CPointer_toWord (Pointer p);

MLTON_CODEGEN_STATIC_INLINE
Pointer CPointer_add (Pointer p, C_Size_t s) {
  return (p + s);
}
MLTON_CODEGEN_STATIC_INLINE
C_Size_t CPointer_diff (Pointer p1, Pointer p2) {
  return (p1 - p2);
}
MLTON_CODEGEN_STATIC_INLINE
Bool CPointer_equal (Pointer p1, Pointer p2) {
  return (p1 == p2);
}
MLTON_CODEGEN_STATIC_INLINE
Pointer CPointer_fromWord (C_Pointer_t x) {
  return (Pointer)x;
}
MLTON_CODEGEN_STATIC_INLINE
Bool CPointer_lt (Pointer p1, Pointer p2) {
  return (p1 < p2);
}
MLTON_CODEGEN_STATIC_INLINE
Pointer CPointer_sub (Pointer p, C_Size_t s) {
  return (p - s);
}
MLTON_CODEGEN_STATIC_INLINE
C_Pointer_t CPointer_toWord (Pointer p) {
  return (C_Pointer_t)p;
}
