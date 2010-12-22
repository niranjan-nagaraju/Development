#include "ip_utils.h"

int main(void)
{
	char str[1000] = {0};
	char ip_to_check[30] = {0};
	uint32_t ip;
	ip_ranges_t ranges;
	int i;

	printf("Enter IP Ranges list: \n");
	scanf("%[^\n]", str);

	printf("Enter IP address to be validated: ");
	scanf("%s", ip_to_check);
	
	ranges = string_to_ip_ranges(str);

	printf("\n IP ranges : %d\n", ranges.ips_num);
	for(i=0; i<ranges.ips_num; i++) {
		printf("\t0x%X - 0x%X\n", ranges.ips[i].lb, ranges.ips[i].ub);
	}

	ip = ipaddress_from_string(ip_to_check);
	printf("Checking if 0x%X(%s) is permitted\n", ip, ip_to_check);

	if (is_ip_in_range(ranges, ip) != -1) {
		printf("IP address %s is in range(0x%X - 0x%X)\n", ip_to_check, ranges.ips[i].lb, ranges.ips[i].ub);
	} else {
		printf("IP address %s is NOT in range\n", ip_to_check);
	}

	return 0;
}
