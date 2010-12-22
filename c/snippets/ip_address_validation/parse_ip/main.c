#include "ip_utils.h"
#include <arpa/inet.h>

int main(void)
{
	char str[] = "192.168.1.4 - 192.168.1.10, 192.168.1.1 - 192.168.1.2, 192.168.2.3";
	char ip_to_check[] = "192.168.1.5";
	uint32_t ip;

	ip_ranges_t ranges;
	int i;
	
	ranges = string_to_ip_ranges(str);

	/**
	if (0x01020304 <= 0x01020305 && 0x0102030A >= 0x01020305)
		printf("%x In range\n", 0x01020305);
	*/

	printf("\n In Main.. IP ranges : %d\n", ranges.ips_num);

	ip = ipaddress_from_string(ip_to_check);
	printf("Checking if 0x%X is permitted\n", ip);

	for (i=0; i<ranges.ips_num; i++) {
		printf("0x%X - 0x%X\n", ranges.ips[i].lb, ranges.ips[i].ub);
		if (ranges.ips[i].lb <= ip && ranges.ips[i].ub >= ip) {
			printf("In range\n");
		}
	}

	printf("%X\n", htonl(ipaddress_from_string("192.168.1.2")));



	return 0;
}
