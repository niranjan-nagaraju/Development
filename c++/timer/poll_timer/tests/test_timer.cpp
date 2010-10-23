#include <timer.h>
#include <iostream>

using namespace std;
using namespace libraries::timer;

void cb1(void *args)
{
	cout<<time(NULL)<<": Callback1"<<endl;
}

void cb2(void *args)
{
	cout<<time(NULL)<<": Callback2"<<endl;
}

void cb3(void *args)
{
	long long li = reinterpret_cast<long long>(args);
	int i = static_cast<int>(li);

	cout<<time(NULL)<<": Callback3, Argument: "<<i<<endl;
}

int main(void)
{
	libraries::timer::timer_t mytimer(5);

	cout<<"Current Time: "<<time(NULL)<<endl;

	mytimer.add(time(NULL)+10, cb1);
	mytimer.add(time(NULL)+5, cb1);
	mytimer.add(time(NULL)+20, cb3, (void *)42);
	mytimer.add(time(NULL)+5, cb2);
	mytimer.add(time(NULL)+15, cb1);
	mytimer.add(time(NULL)+35, cb3, (void *)45);
	mytimer.add(time(NULL)+34, cb3, (void *)35);

	sleep(40);
	exit(1);
}
