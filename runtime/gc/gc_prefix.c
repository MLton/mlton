#include "gc.h"

static inline size_t maxZ (size_t x, size_t y) {
  return ((x < y) ? x : y);
}

static inline size_t meg (size_t n) {
  return n / (1024ul * 1024ul);
}

