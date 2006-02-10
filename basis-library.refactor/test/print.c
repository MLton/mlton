#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

void printInt8 (int8_t i) {
  printf("%"PRId8"\n", i);
}

void printInt16 (int16_t i) {
  printf("%"PRId16"\n", i);
}

void printInt32 (int32_t i) {
  printf("%"PRId32"\n", i);
}

void printInt64 (int64_t i) {
  printf("%"PRId64"\n", i);
}

void printWord8 (uint8_t i) {
  printf("%"PRIx8"\n", i);
}

void printWord16 (uint16_t i) {
  printf("%"PRIx16"\n", i);
}

void printWord32 (uint32_t i) {
  printf("%"PRIx32"\n", i);
}

void printWord64 (uint64_t i) {
  printf("%"PRIx64"\n", i);
}
