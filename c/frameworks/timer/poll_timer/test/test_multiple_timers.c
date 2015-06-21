#include <poll_timer.h>

struct operands
{
	int a;
	int b;
};

void timer_fn1(void *args)
{
	struct operands *ops = (struct operands *)args;
	int i;
	int currtime = time(NULL);
	static int times = 1;

	for (i=0; i<4; i++) {
		ops[i].a += ops[i].a;
		ops[i].b += 2*(ops[i].b);
	}

	printf("Timer1 %d -> %d\n", currtime, times++);
	for (i=0; i<4; i++)
		printf("(%d, %d) ", ops[i].a, ops[i].b);
	printf("\n");
}

void timer_fn2(void *args)
{
	int *num = (int *)args;
	int currtime = time(NULL);
	static int times = 1;

	printf("Timer2 %d -> %d Number = %d\n", currtime, times++, *num);
	*num += 10;
}

int main(void)
{
	struct operands ops[4] = {{1,2}, {2,3}, {3,4}, {4,5}};
	int number = 5;
	int currtime = time(NULL);
	int i;

	/** Recur 10 times at 10 seconds interval */
	timer_t *timer1 = set_timer(10, 1, 10, 5, timer_fn1, (void *)ops);

	/** Recur twice at 1 mins interval */
	timer_t *timer2 = set_timer(60, 1, 2, 10, timer_fn2, (void *)(&number));

	printf("Starting timers at %d\n", currtime);

	start_timer(timer1);
	start_timer(timer2);

	pthread_join(timer1->thread, NULL);
	pthread_join(timer2->thread, NULL);

	printf("Results of timer 1: \n");
	for (i=0; i<4; i++)
		printf("(%d, %d) ", ops[i].a, ops[i].b);

	printf("\nResults of timer 2: %d\n", number);

	printf("Exiting\n");

	return 0;
}
