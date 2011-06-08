#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define MAXN 65536
#define MASK 65535

int a[MAXN];
int masks[16];

unsigned bitset[MAXN >> 5];

inline int readint()
{
    int ch, ret = 0;

    while (!isdigit(ch = getchar_unlocked()));
    do {
        ret = ret * 10 + ch - '0';
    } while (isdigit(ch = getchar_unlocked()));

    return ret;
}

int main()
{
    int cas = readint(), casNum;
    
    for (casNum = 1; casNum <= cas; casNum++) {
        int n = readint();

        int orBase = MASK;
        int i, j;

        for (i = 0; i < n; i++) {
            a[i] = readint();
            orBase &= a[i];
        }

        for (i = 0; i < 16; i++) {
            int andValue = MASK + MAXN;
            int found = 0;
            for (j = 0; j < n; j++)
                if (a[j] >> i & 1)
                    andValue &= a[j];
            masks[i] = andValue;
        }

        int num = 0;
        for (i = 0; i < 16; i++) {
            int found = 0;
            if (masks[i] & MAXN)
                found = 1;
            for (j = 0; j < num; j++)
                if (masks[i] == masks[j])
                    found = 1;
            if (!found) {
                masks[num++] = masks[i];
            }
        }

        memset(bitset, 0, sizeof(bitset));
        bitset[orBase >> 5] |= 1U << (orBase & 31);

        for (i = 0; i < num; i++) {
            int msk = masks[i];
            for (j = MASK >> 5; j >= 0; j--) {
                unsigned val = bitset[j];
                if (msk & 1) {
                    val = (val & 0xaaaaaaaaU) | (val & 0x55555555U) << 1;
                }
                if (msk & 2) {
                    val = (val & 0xccccccccU) | (val & 0x33333333U) << 2;
                }
                if (msk & 4) {
                    val = (val & 0xf0f0f0f0U) | (val & 0x0f0f0f0fU) << 4;
                }
                if (msk & 8) {
                    val = (val & 0xff00ff00U) | (val & 0x00ff00ffU) << 8;
                }
                if (msk & 16) {
                    val = (val & 0xffff0000U) | (val & 0x0000ffffU) << 16;
                }
                bitset[j | msk >> 5] |= val;
            }
        }

        int bitCounts = 0;
        for (i = 0; i < MAXN >> 5; i++)
            bitCounts += __builtin_popcount(bitset[i]);

        printf("Case #%d: %d\n", casNum, bitCounts - n);
    }

    return 0;
}
