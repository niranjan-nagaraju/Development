#ifndef __IP_UTILS_H__

#define __IP_UTILS_H__
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_RANGES 100

typedef struct ip_range_s {
	uint32_t lb, ub;
} ip_range_t;

typedef struct ip_ranges_s {
	int ips_num;
	ip_range_t ips[MAX_RANGES];
} ip_ranges_t;


uint32_t ipaddress_from_string(char *ipstr);
ip_ranges_t string_to_ip_ranges(char *ipranges_list);
int is_ip_in_range(ip_ranges_t ranges, uint32_t ip);

#endif
