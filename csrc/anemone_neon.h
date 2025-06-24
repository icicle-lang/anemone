#ifndef __ANEMONE_NEON_H
#define __ANEMONE_NEON_H

#if !defined(__ARM_NEON) && !defined(__ARM_NEON__)
#error "You must compile with NEON enabled!"
#endif

#include "anemone_base.h"
#include <arm_neon.h>
#include <stdint.h>
#include <string.h>


ANEMONE_INLINE
uint8x16_t anemone_neon_load_bytes128(const char *start, const char *end) {
    int len = end - start;
    if (len >= 16) {
        return vld1q_u8((const uint8_t*)start);
    } else {
        uint8_t tmp[16] = {0};
        memcpy(tmp, start, len);
        return vld1q_u8((const uint8_t*) tmp);
    }
}

// Sums 4 32-bit elements in a NEON vector
ANEMONE_INLINE
uint64_t anemone_neon_sum1_epi32(const uint32x4_t a) {
    return (uint64_t) vaddvq_u32(a);
}

// Sums 8 32-bit elements in two NEON vectors
ANEMONE_INLINE
uint64_t anemone_neon_sum2_epi32(const uint32x4_t a, const uint32x4_t b) {
    return (uint64_t) vaddvq_u32(vaddq_u32(a, b));
}

// Finds the index of the first non-digit byte ('0'-'9') in a vector
ANEMONE_INLINE
unsigned int anemone_neon_first_nondigit(const uint8x16_t m) {
    uint8_t bytes[16];
    vst1q_u8(bytes, m);
    for (unsigned int i = 0; i < 16; ++i) {
        if (bytes[i] < '0' || bytes[i] > '9') {
            return i;
        }
    }
    return 16;
}

// Computes indices of the first non-digit and first non-zero bytes in a vector
ANEMONE_INLINE
void anemone_neon_first_nondigit_first_nonzero(
    const uint8x16_t m, unsigned int *out_first_nondigit, unsigned int *out_first_nonzero) {
    uint8_t bytes[16];
    vst1q_u8(bytes, m);
    unsigned int nondigit = 16, nonzero = 16;

    // Use a single iterator for both loops.
    unsigned int i = 0;
    for (; i < 16; ++i) {
        if (bytes[i] != '0') {
            nonzero = i;
            break;
        }
    }
    for (; i < 16; ++i) {
        if ((bytes[i] < '0' || bytes[i] > '9')) {
            nondigit = i;
            break;
        }
    }
    *out_first_nondigit = nondigit;
    *out_first_nonzero = nonzero;
}

#endif // __ANEMONE_NEON_H