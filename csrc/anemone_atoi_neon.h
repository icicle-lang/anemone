#include "anemone_neon.h"

/*
 compute digit of first eight chars in m.

 m = [ c0, c1, c2, c3, c4, c5, c6, c7, ... c15 ]@8

 index is index of first non-digit; that is, where the number ends

 something like this:

     return c0 * pow10[index-1]
          + c1 * pow10[index-2]
          + ...
          + c7 * pow10[index-8];

 where pow10[0] = 1, pow10[<0] = 0, pow10[1] = 10, ...
 */
ANEMONE_INLINE
uint64_t anemone_string_to_i64_neon_first_eight(const uint8x16_t m, unsigned int index) {
    static const uint32_t powers_of_ten_multipliers[]
     = { 10000000, 1000000, 100000, 10000
       , 1000    , 100    , 10    , 1
       , 0       , 0      , 0     , 0
       , 0       , 0      , 0     , 0
       , 0       , 0      , 0     , 0
       , 0       , 0      , 0     , 0
       };

    // m      = [ c0, c1, c2, c3, c4, c5, c6, c7, ... c15 ]@8
    // Extend bytes c0-c7 into 32-bit integers, storing in two vectors:
    // lows   = [ c0, c1, c2, c3 ]@32
    // highs  = [ c4, c5, c6, c7 ]@32

    // First extract and extend the bottom 8 bytes and extend;
    uint16x8_t lower     = vmovl_u8(vget_low_u8(m));
    // Then extract the lows and highs from that, extending also.
    uint32x4_t lows      = vmovl_u16(vget_low_u16(lower));
    uint32x4_t highs     = vmovl_u16(vget_high_u16(lower));

    // Compute power of ten multipliers for each digit
    const int pow_ten_offset = (index < 8) ? 8 - index : 0;

    uint32x4_t lo_muls   = vld1q_u32(powers_of_ten_multipliers + pow_ten_offset);
    uint32x4_t hi_muls   = vld1q_u32(powers_of_ten_multipliers + pow_ten_offset + 4);

    // Multiply each 32-bit extended integer against its power of ten
    uint32x4_t lo_mulled = vmulq_u32(lo_muls, lows);
    uint32x4_t hi_mulled = vmulq_u32(hi_muls, highs);

    // Sum all together
    uint64_t sum = anemone_neon_sum2_epi32(lo_mulled, hi_mulled);
    return sum;
}

ANEMONE_INLINE
int64_t anemone_string_to_ui64_v128_floating(char **pp, char *pe, uint64_t *out_val, int64_t *out_exponent_spill, int64_t *out_significant_digits) {
    static const uint32_t powers_of_ten[]
         = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000};


    char* in = *pp;
    if (pe - in == 0) {
        return 1;
    }

    // Load string into vector: vector stores 16 bytes.
    // This is a *safe* read and anything past the end of the buffer will be null
    // Which means it will be automatically ignored by the nondigit check
    // > m = [ '1', '0', 'x', ... ]@8
    uint8x16_t m = anemone_neon_load_bytes128(in, pe);

    // Find first non-digit and first non-zero.
    // The first non-zero is used to remove leading zeros.
    // We assume the common case has no leading zeros.
    // > anemone_sse_first_nondigit( [ '1', '0', 'x', ... ]) = 2
    unsigned int index;
    unsigned int nonzero;
    anemone_neon_first_nondigit_first_nonzero(m, &index, &nonzero);

    // Handle leading zeros
    while (nonzero > 0) {
        if (nonzero >= index && index != 16) {
            *out_val = 0;
            *pp = in + index;
            *out_exponent_spill = 0;
            *out_significant_digits = 1;
            return 0;
        }

        if (nonzero == 16) nonzero = 15;
        in += nonzero;
        m   = anemone_neon_load_bytes128(in, pe);
        anemone_neon_first_nondigit_first_nonzero(m, &index, &nonzero);
    }

    in += index;

    // Convert characters to integers
    const uint8x16_t zeros = vdupq_n_u8(48);
    m                      = vsubq_u8(m, zeros);

    // Convert first 8 bytes
    uint64_t int_out = anemone_string_to_i64_neon_first_eight(m, index);

    uint64_t exponent_spill = 0;
    uint64_t significant_digits = index;

    if (ANEMONE_UNLIKELY(index == 0)) {
        return 1;
    }

    if (ANEMONE_UNLIKELY(index > 8)) {
        m = vextq_u8(m, m, 8);
        // Shift right to get second half
        const uint64_t i2 = anemone_string_to_i64_neon_first_eight(m, index - 8);
        // Convert the second half of the vector
        const uint64_t mul2 = powers_of_ten[index - 8];
        int_out = int_out * mul2 + i2;


        // If index == 16, it means the vector contains only digits.
        // So we need to load the next 16 bytes to find the end of the number
        if (ANEMONE_UNLIKELY(index == 16)) {
            m     = anemone_neon_load_bytes128(in, pe);
            index = anemone_neon_first_nondigit(m);
            in   += index;

            // We can only hold 19 digits (16+3 == 19)
            // So if it's any more we would overflow - count how many extra digits there are
            if (index > 3) {
                exponent_spill = index - 3;
                while (index == 16) {
                    uint8x16_t throw = anemone_neon_load_bytes128(in, pe);
                    index            = anemone_neon_first_nondigit(throw);
                    in              += index;
                    exponent_spill  += index;
                }
                // Finally, pretend there were only 19 digits.
                index = 3;
            }
            significant_digits += index;

            // For total digits > 16 and <= 19, we just need to perform a final fixup and join
            m                   = vsubq_u8(m, zeros);
            const uint64_t i3   = anemone_string_to_i64_neon_first_eight(m, index);
            const uint64_t mul3 = powers_of_ten[index];
            int_out = int_out * mul3 + i3;
        }
    }

    *out_val = int_out;
    *pp = in;
    *out_exponent_spill = exponent_spill;
    *out_significant_digits = significant_digits;
    return 0;
}