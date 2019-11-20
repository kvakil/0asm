#include <stdint.h>
#include <stdio.h>

uint16_t hash(char *s) {
    uint16_t ax = *s++, bx, cx = 0x23;
    do {
        bx = cx;
        cx <<= 5;
        cx -= bx;
        cx += ax;
        ax = *s++;
    } while (ax > 0x40);
    return cx;
}

int main(int argc, char **argv) {
    for (int i = 1; i < argc; i++) {
        printf("%s: 0o%06o\n", argv[i], hash(argv[i]));
    }
}
