#include "anemone_atoi_v128.h"
#if defined(__SSE4_2__)
#include "anemone_atoi_sse.h"
#else
#include "anemone_atoi_neon.h"
#endif
/*
  parse unsigned 64-bit integer.
 */
ANEMONE_INLINE
ANEMONE_STATIC
error_t anemone_string_to_ui64_v128 (char **pp, char *pe, uint64_t *out_val)
{
    char* pp_ = *pp;
    int64_t exp_spill;
    int64_t digits;
    int64_t ret = anemone_string_to_ui64_v128_floating (&pp_, pe, out_val, &exp_spill, &digits);

    if (ret == 0 && exp_spill == 0) {
        *pp = pp_;
        return 0;
    } else {
        return 1;
    }
}

/*
  parse string to signed 64-bit integer.
  does not strip whitespace.
 */
error_t anemone_string_to_i64_v128 (char **pp, char *pe, int64_t *out_val)
{
    char* in = *pp;
    uint64_t buffer_size = pe - in;
    if (buffer_size == 0) {
        return 1;
    }

    int64_t sign             = 1;

    if (in[0] == '-') {
        in++;
        sign                 = -1;
    }
    uint64_t int_out;
    if (anemone_string_to_ui64_v128(&in, pe, &int_out)) {
      return 1;
    }

    // Check whether it would overflow in converting to a signed int64
    if (ANEMONE_UNLIKELY(sign == 1 && int_out > INT64_MAX)) {
        return 1;
    } else if (ANEMONE_UNLIKELY(sign == -1 && int_out > (uint64_t)INT64_MAX + 1)) {
        return 1;
    }

    *pp = in;
    *out_val = int_out * sign;
    return 0;
}
