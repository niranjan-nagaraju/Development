#include <iostream>
using namespace std;

class A {
	public:
	void x(void) {
		int z = 10;
		delete this;

		cout<<z<<endl;
	}
};

int main(void)
{
	A *a= new A;
	a->x();

	return 0;
}
