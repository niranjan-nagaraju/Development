#include <ip_utils.h>

int
is_ip_in_range(ip_ranges_t ranges, uint32_t ip)
{
	int i;

	for (i=0; i<ranges.ips_num; i++) {
		printf("Checking 0x%X - 0x%X\n", ranges.ips[i].lb, ranges.ips[i].ub);
		if (ranges.ips[i].lb <= ip && ranges.ips[i].ub >= ip) {
			return i;
		}
	}

	return -1;
}
