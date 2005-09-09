#include "gc.h"

static inline size_t maxZ (size_t x, size_t y) {
  return ((x < y) ? x : y);
}
