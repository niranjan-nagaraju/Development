#include <iostream>
using namespace std;

class Singleton
{
	private:
		int a, b;
		static Singleton *instance;

		Singleton()
		{
			this->a  = 1;
			this->b = 2;
		}

	public:
		static Singleton* make_object(void)
		{
			if (!instance)
				instance = new Singleton();

			return instance;
		}

		void print(void)
		{
			cout<<a<<' '<<b<<endl;
		}

};

Singleton *Singleton::instance = 0;

int main(void)
{
	Singleton *x = Singleton::make_object();

	x->print();

	return 0;
}
