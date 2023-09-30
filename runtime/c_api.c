#include "libregexp.h"

int lre_check_stack_overflow(void *opaque, size_t alloca_size)
{
    return 0;
}

void *lre_realloc(void *opaque, void *ptr, size_t size)
{
    return GC_realloc(ptr, size);
}

void format_f64(double f, int *len, char * buf){
    *len = sprintf(buf, "%f", f);
    return;
}