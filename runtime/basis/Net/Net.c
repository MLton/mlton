#include "platform.h"

Word32_t Net_htonl (Word32_t w) {
  Word32_t r = htonl (w);
  return r;
}

Word32_t Net_ntohl (Word32_t w) {
  Word32_t r = ntohl (w);
  return r;
}

Word16_t Net_htons (Word16_t w) {
  Word16_t r = htons (w);
  return r;
}

Word16_t Net_ntohs (Word16_t w) {
  Word16_t r = ntohs (w);
  return r;
}
