#include <iostream>
using namespace std;

/** 
 * Implement a singleton pattern using references
 * Meyer's singleton:
 *   Avoids having to initialize the static instance member outside 
 *   (like in the pointers-based implementation)
 */
class Singleton
{
	private:
		int a, b;

		Singleton()
		{
			this->a  = 1;
			this->b = 2;
		}

		/* Hide copy constructor and '=' */
		Singleton (const Singleton&);
		Singleton operator=(const Singleton&);

	public:
		static Singleton& make_object(void)
		{
			static Singleton instance;
			return instance;
		}

		void print(void)
		{
			cout<<a<<' '<<b<<endl;
		}

};

int main(void)
{
	Singleton &x = Singleton::make_object();

	x.print();

	return 0;
}
