#define MLTON_GC_INTERNAL_TYPES
#define MLTON_GC_INTERNAL_BASIS
#include "platform.h"


PRIVATE objptr IntInf_add (GC_state s, objptr lhs, objptr rhs, size_t bytes);
PRIVATE objptr IntInf_andb (GC_state s, objptr lhs, objptr rhs, size_t bytes);
PRIVATE objptr IntInf_gcd (GC_state s, objptr lhs, objptr rhs, size_t bytes);
PRIVATE objptr IntInf_mul (GC_state s, objptr lhs, objptr rhs, size_t bytes);
PRIVATE objptr IntInf_quot (GC_state s, objptr lhs, objptr rhs, size_t bytes);
PRIVATE objptr IntInf_orb (GC_state s, objptr lhs, objptr rhs, size_t bytes);
PRIVATE objptr IntInf_rem (GC_state s, objptr lhs, objptr rhs, size_t bytes);
PRIVATE objptr IntInf_sub (GC_state s, objptr lhs, objptr rhs, size_t bytes);
PRIVATE objptr IntInf_xorb (GC_state s, objptr lhs, objptr rhs, size_t bytes);
PRIVATE objptr IntInf_neg (GC_state s, objptr arg, size_t bytes);
PRIVATE objptr IntInf_notb (GC_state s, objptr arg, size_t bytes);
PRIVATE objptr IntInf_arshift (GC_state s, objptr arg, Word32_t shift, size_t bytes);
PRIVATE objptr IntInf_lshift (GC_state s, objptr arg, Word32_t shift, size_t bytes);
PRIVATE Int32_t IntInf_compare (GC_state s, objptr lhs, objptr rhs);
PRIVATE objptr IntInf_toString (GC_state s, objptr arg, Int32_t base, size_t bytes);


objptr IntInf_add (GC_state s, objptr lhs, objptr rhs, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_add ("FMTOBJPTR", "FMTOBJPTR", %"PRIuMAX")\n",
             lhs, rhs, (uintmax_t)bytes);
  return IntInf_binop (s, lhs, rhs, bytes, &mpz_add);
}

objptr IntInf_andb (GC_state s, objptr lhs, objptr rhs, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_andb ("FMTOBJPTR", "FMTOBJPTR", %"PRIuMAX")\n",
             lhs, rhs, (uintmax_t)bytes);
  return IntInf_binop (s, lhs, rhs, bytes, &mpz_and);
}

objptr IntInf_gcd (GC_state s, objptr lhs, objptr rhs, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_gcd ("FMTOBJPTR", "FMTOBJPTR", %"PRIuMAX")\n",
             lhs, rhs, (uintmax_t)bytes);
  return IntInf_binop (s, lhs, rhs, bytes, &mpz_gcd);
}

objptr IntInf_mul (GC_state s, objptr lhs, objptr rhs, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_mul ("FMTOBJPTR", "FMTOBJPTR", %"PRIuMAX")\n",
             lhs, rhs, (uintmax_t)bytes);
  return IntInf_binop (s, lhs, rhs, bytes, &mpz_mul);
}

objptr IntInf_quot (GC_state s, objptr lhs, objptr rhs, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_quot ("FMTOBJPTR", "FMTOBJPTR", %"PRIuMAX")\n",
             lhs, rhs, (uintmax_t)bytes);
  return IntInf_binop (s, lhs, rhs, bytes, &mpz_tdiv_q);
}

objptr IntInf_orb (GC_state s, objptr lhs, objptr rhs, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_orb ("FMTOBJPTR", "FMTOBJPTR", %"PRIuMAX")\n",
             lhs, rhs, (uintmax_t)bytes);
  return IntInf_binop (s, lhs, rhs, bytes, &mpz_ior);
}

objptr IntInf_rem (GC_state s, objptr lhs, objptr rhs, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_quot ("FMTOBJPTR", "FMTOBJPTR", %"PRIuMAX")\n",
             lhs, rhs, (uintmax_t)bytes);
  return IntInf_binop (s, lhs, rhs, bytes, &mpz_tdiv_r);
}

objptr IntInf_sub (GC_state s, objptr lhs, objptr rhs, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_sub ("FMTOBJPTR", "FMTOBJPTR", %"PRIuMAX")\n",
             lhs, rhs, (uintmax_t)bytes);
  return IntInf_binop (s, lhs, rhs, bytes, &mpz_sub);
}

objptr IntInf_xorb (GC_state s, objptr lhs, objptr rhs, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_xorb ("FMTOBJPTR", "FMTOBJPTR", %"PRIuMAX")\n",
             lhs, rhs, (uintmax_t)bytes);
  return IntInf_binop (s, lhs, rhs, bytes, &mpz_xor);
}


objptr IntInf_neg (GC_state s, objptr arg, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_neg ("FMTOBJPTR", %"PRIuMAX")\n",
             arg, (uintmax_t)bytes);
  return IntInf_unop (s, arg, bytes, &mpz_neg);
}

objptr IntInf_notb (GC_state s, objptr arg, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_notb ("FMTOBJPTR", %"PRIuMAX")\n",
             arg, (uintmax_t)bytes);
  return IntInf_unop (s, arg, bytes, &mpz_com);
}


objptr IntInf_arshift (GC_state s, objptr arg, Word32_t shift, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_arshift ("FMTOBJPTR", %"PRIu32", %"PRIuMAX")\n",
             arg, shift, (uintmax_t)bytes);
  return IntInf_shop (s, arg, shift, bytes, &mpz_fdiv_q_2exp);
}

objptr IntInf_lshift (GC_state s, objptr arg, Word32_t shift, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_lshift ("FMTOBJPTR", %"PRIu32", %"PRIuMAX")\n",
             arg, shift, (uintmax_t)bytes);
  return IntInf_shop (s, arg, shift, bytes, &mpz_mul_2exp);
}


Int32_t IntInf_compare (GC_state s, objptr lhs, objptr rhs) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_compare ("FMTOBJPTR", "FMTOBJPTR")\n",
             lhs, rhs);
  return IntInf_cmpop (s, lhs, rhs, &mpz_cmp);
}


objptr IntInf_toString (GC_state s, objptr arg, Int32_t base, size_t bytes) {
  if (DEBUG_INT_INF)
    fprintf (stderr, "IntInf_toString ("FMTOBJPTR", %"PRId32", %"PRIuMAX")\n",
             arg, base, (uintmax_t)bytes);
  return IntInf_strop (s, arg, base, bytes, &mpz_get_str);
}
