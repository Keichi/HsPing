#include <stdio.h>
#include <stdint.h>

unsigned short checksum(unsigned short *buf, int size)
{
	unsigned long sum = 0;

	while (size > 1) {
		sum += *buf++;
		size -= 2;
	}
	if (size)
		sum += *(char *)buf;

	sum  = (sum & 0xffff) + (sum >> 16);	/* add overflow counts */
	sum  = (sum & 0xffff) + (sum >> 16);	/* once again */
	
	return ~sum;
}

int main() {
    char buf[5];
    buf[0] = 1;
    buf[1] = 2;
    buf[2] = 3;
    buf[3] = 4;
    buf[4] = 5;

    unsigned short chk = checksum((short*)buf, 5);

    printf("%d\n", chk);

    return 0;
}
