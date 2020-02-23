#include <iostream>
using namespace std;

/** 
 * Implement a singleton pattern using pointers
 */
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

		/* Hide copy constructor and '=' */
		Singleton (const Singleton&);
		Singleton operator=(const Singleton&);

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
