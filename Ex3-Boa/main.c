#include <stdio.h>
#include <stdint.h>

extern int64_t program_entry_point() asm("program_entry_point");

int main(void)
{
    int64_t result = program_entry_point();
    printf("%ld\n", result);
}
