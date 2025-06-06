#ifndef __ANEMONE_ATOI_SSE_H
#define __ANEMONE_ATOI_SSE_H

#include "anemone_base.h"

/*
  parse string to signed 64-bit integer.
  does not strip whitespace.
 */
error_t anemone_string_to_i64_v128 (char **pp, char *pe, int64_t *out_val);

#endif//__ANEMONE_ATOI_SSE_H
