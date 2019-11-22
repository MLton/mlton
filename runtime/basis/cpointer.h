PRIVATE INLINE
Pointer CPointer_add (Pointer p, C_Size_t s);
PRIVATE INLINE
C_Size_t CPointer_diff (Pointer p1, Pointer p2);
PRIVATE INLINE
Bool CPointer_equal (Pointer p1, Pointer p2);
PRIVATE INLINE
Pointer CPointer_fromWord (C_Pointer_t x);
PRIVATE INLINE
Bool CPointer_lt (Pointer p1, Pointer p2);
PRIVATE INLINE
Pointer CPointer_sub (Pointer p, C_Size_t s);
PRIVATE INLINE
C_Pointer_t CPointer_toWord (Pointer p);

PRIVATE INLINE
Pointer CPointer_add (Pointer p, C_Size_t s) {
  return (p + s);
}
PRIVATE INLINE
C_Size_t CPointer_diff (Pointer p1, Pointer p2) {
  return (size_t)(p1 - p2);
}
PRIVATE INLINE
Bool CPointer_equal (Pointer p1, Pointer p2) {
  return (p1 == p2);
}
PRIVATE INLINE
Pointer CPointer_fromWord (C_Pointer_t x) {
  return (Pointer)x;
}
PRIVATE INLINE
Bool CPointer_lt (Pointer p1, Pointer p2) {
  return (p1 < p2);
}
PRIVATE INLINE
Pointer CPointer_sub (Pointer p, C_Size_t s) {
  return (p - s);
}
PRIVATE INLINE
C_Pointer_t CPointer_toWord (Pointer p) {
  return (C_Pointer_t)p;
}
