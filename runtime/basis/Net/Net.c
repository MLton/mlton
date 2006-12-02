#include "platform.h"

#ifndef DEBUG
#define DEBUG FALSE
#endif

Word32_t Net_htonl (Word32_t w) {
  Word32_t r = htonl (w);
  if (DEBUG)
    printf ("%"PRIx32" = Net_htonl (%"PRIx32")\n", r, w);
  return r;
}

Word32_t Net_ntohl (Word32_t w) {
  Word32_t r = ntohl (w);
  if (DEBUG)
    printf ("%"PRIx32" = Net_ntohl (%"PRIx32")\n", r, w);
  return r;
}

Word16_t Net_htons (Word16_t w) {
  Word16_t r = htons (w);
  if (DEBUG)
    printf ("%"PRIx16" = Net_htonl (%"PRIx16")\n", r, w);
  return r;
}

Word16_t Net_ntohs (Word16_t w) {
  Word16_t r = ntohs (w);
  if (DEBUG)
    printf ("%"PRIx16" = Net_ntohl (%"PRIx16")\n", r, w);
  return r;
}
