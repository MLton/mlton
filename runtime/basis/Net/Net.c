#include "platform.h"

Word32_t Net_htonl (Word32_t w) {
  return htonl (w);
}

Word32_t Net_ntohl (Word32_t w) {
  return ntohl (w);
}

Word16_t Net_htons (Word16_t w) {
  return htons (w);
}

Word16_t Net_ntohs (Word16_t w) {
  return ntohs (w);
}
