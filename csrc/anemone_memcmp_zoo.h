#ifndef __ANEMONE_MEMCMP_ZOO_H
#define __ANEMONE_MEMCMP_ZOO_H

/* Anemone memory comparison functions.
 * A zoo of implementations for benchmarking against.
 */

#include "anemone_base.h"
#include "anemone_twiddle.h"
#include "anemone_memcmp.h"

#if defined(__SSE4_2__)
#include "anemone_sse.h"
#endif


ANEMONE_INLINE
int anemone_memcmp8 (const void *as, const void *bs, size_t len)
{
    for (uint64_t i = 0; i != len; ++i) {
        char a = *(char *)(as + i);
        char b = *(char *)(bs + i);
        if (a != b) {
            /* We are doing an unsigned comparison, so:
             * make sure we cast to an unsigned char
             * then move into a signed 64-bit to hold negative result */
            int64_t a_i = (uint8_t)a;
            int64_t b_i = (uint8_t)b;
            return a_i - b_i;
        }
    }

    return 0;
}


ANEMONE_INLINE
int anemone_memcmp_partial_load64 (const void *as, const void *bs, size_t len)
{
    uint64_t rem = len;
    while (rem >= 8) {
        uint64_t a = *(uint64_t*)as;
        uint64_t b = *(uint64_t*)bs;

        if (a != b) {
            /* if they are different, we need to know which is bigger */
            uint64_t a_s = anemone_bswap64(a);
            uint64_t b_s = anemone_bswap64(b);
            if (a_s > b_s) {
              return 1;
            } else {
              return -1;
            }
        }

        rem -= 8;
        as += 8;
        bs += 8;
    }

    uint64_t a = anemone_bswap64(anemone_partial_load64(as, rem));
    uint64_t b = anemone_bswap64(anemone_partial_load64(bs, rem));
    if (a > b) {
        return 1;
    } else if (a < b) {
        return -1;
    } else {
        return 0;
    }
}


ANEMONE_INLINE
int anemone_memcmp128_unsafe (const void *buf1, const void *buf2, size_t len)
{
#if defined(__SSE4_2__)

    /* Loop through the buffers in chunks of 16 */
    while (len > 0) {
        /* Take 16, or if this is the last chunk, the remnants */
        size_t current_chunk = (len < 16) ? len : 16;
        /* Load the values into registers */

        __m128i m1 = _mm_loadu_si128((__m128i*)buf1);
        __m128i m2 = _mm_loadu_si128((__m128i*)buf2);

        /* Find the index of the first byte where the two chunks differ */
        unsigned int index = _mm_cmpestri(m1, current_chunk, m2, current_chunk
               , _SIDD_UBYTE_OPS            /* Compare unsigned bytes */
               | _SIDD_CMP_EQUAL_EACH       /* Pair-wise equality */
               | _SIDD_NEGATIVE_POLARITY);  /* Not equal instead of equal */

        /* If the index is 16 or current_chunk, they are equal */
        if (index < current_chunk) {
            /* If the index is less than current chunk, buf1[index] and buf2[index] are different */
            /* Get characters as ints so we can do signed subtract */
            int char1 = *(uint8_t *)(buf1 + index);
            int char2 = *(uint8_t *)(buf2 + index);
            /* Return the difference of the two */
            return char1 - char2;
        }

        /* Move the buffers ahead by 16 */
        buf1 += 16;
        buf2 += 16;
        /* Subtract the length. current_chunk <= len, so if last chunk, len = 0*/
        len -= current_chunk;
    }

    /* Got to the end with no differences */
    return 0;

#else
    return anemone_memcmp(buf1, buf2, len);
#endif
}


#endif//__ANEMONE_MEMCMP_ZOO_H
