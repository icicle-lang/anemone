#include "anemone_atoi.h"
#include "anemone_atoi_v128.h"
#include <stdlib.h>

#define NUM_BENCHES 1000000
#define MKBENCH(f) \
int64_t f##_bench(char* str, uint64_t len)                  \
{                                                           \
    uint64_t num = 0;                                       \
    for (uint64_t i = 0; i != NUM_BENCHES; ++i) {           \
        char* start = str;                                  \
        char* end = str + len;                              \
        int64_t out;                                        \
        num += f(&start, end, &out);                        \
    }                                                       \
    return num;                                             \
}

MKBENCH(anemone_string_to_i64)
MKBENCH(anemone_string_to_i64_v128)

/* do not inline this so the C compiler cannot do any fancy tricks */
int64_t __attribute__((noinline)) anemone_std_atoi(char **pp, char *pe, int64_t *output_ptr)
{
    int out = atoi(*pp);
    *output_ptr = out;
    return out;
}
MKBENCH(anemone_std_atoi)
