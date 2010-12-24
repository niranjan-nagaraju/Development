#include "ip_utils.h"

uint32_t
ipaddress_from_string(char *ipstr)
{
	unsigned int b1, b2, b3, b4;
	uint32_t ipaddress;
	int ret;

	ret = sscanf(ipstr, "%d.%d.%d.%d", &b1, &b2, &b3, &b4);
	if (ret != 4)
		return 0;

	ipaddress = (b1 << 24) | (b2 << 16) | (b3 <<8) | b4;

	return ipaddress;	
}

static ip_range_t
extract_ipaddress_range(char *range_str)
{
	char *p;
	char range_delim = '-';
	ip_range_t range;

	if ((p = strchr(range_str, range_delim))) {
		*p = '\0';
		printf("lb: %s ub: %s\t", range_str, p+1);
		range.lb = ipaddress_from_string(range_str);
		range.ub = ipaddress_from_string(p+1);
	} else {
		printf("lb: %s ub: %s\t", range_str, "NULL");
		range.lb = ipaddress_from_string(range_str);
		range.ub = 0;
	}

	printf("Integer: (%X) - (%X) [%u] - [%u]\n", range.lb, range.ub, range.lb, range.ub);
	return range;
}


ip_ranges_t
string_to_ip_ranges(char *ipranges_list)
{
	ip_ranges_t ranges;
	char list_separator[] = ",";
	char *range_str;

	ranges.ips_num = 0;

	range_str = strtok(ipranges_list, list_separator);
	while(range_str != NULL) {
		ip_range_t range;

		range = extract_ipaddress_range(range_str);
		if (range.lb != 0) {
			ranges.ips[ranges.ips_num++] = range;
		}

		range_str = strtok(NULL, list_separator);
	} 

	return ranges;
}


