#include <stdio.h>
#include <stdint.h>

extern int64_t compiler_entry_point() asm("compiler_entry_point");

int main(void)
{
    int64_t result = compiler_entry_point();
    printf("%ld\n", result);
}
