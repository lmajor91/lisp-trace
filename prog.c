#include <stdio.h>

int flag = 0xABCD;
unsigned long long x = 0;

int main () {
	while (flag == 0xABCD) {
		++x;
	}
	printf("Modified flag variable! Flag now: %x", flag);
	return 0;
}
