#ifndef __OFFSETOF_H_
#define __OFFSETOF_H_

#define OFFSETOF(T, member) \
	(size_t)&(((T *)0)->member)

#endif
