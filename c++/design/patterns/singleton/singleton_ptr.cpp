#include <iostream>
#include <cassert>

/** 
 * Implement a singleton pattern using pointers
 * Lazy initialization
 *
 * WARNING: *NOT* thread-safe.
 * SEE: 
 *  C++ and the Perils of Double-Checked Locking (Scott Meyers and Andrei Alexandrescu)
 *		https://www.aristeia.com/Papers/DDJ_Jul_Aug_2004_revised.pdf
 */
class Singleton
{
	private:
		static Singleton *instance;

		Singleton()
		{
			this->a  = 0;
			this->b = 0;
		}

		Singleton(int a, int b)
		{
			this->a  = a;
			this->b = b;
		}

		/* Hide copy constructor and '=' */
		Singleton (const Singleton&);
		Singleton operator=(const Singleton&);

	public:
		int a, b;
		static Singleton* make_object(void)
		{
			if (!instance)
				instance = new Singleton();

			return instance;
		}

		static Singleton* make_object(int a, int b)
		{
			instance = make_object();

			instance->a = a;
			instance->b = b;

			return instance;
		}

		void print(void)
		{
			std::cout << a <<' '<< b << std::endl;
		}

};

/** Initialize the static instance to NULL  -- */
Singleton *Singleton::instance = 0;

int main(void)
{
	Singleton *x = Singleton::make_object();
	Singleton *y = Singleton::make_object();

	assert(x->a == y->a && y->a == 0);
	assert(x->b == 0 && y->b == 0);

	/** Underlying Singleton pointers are all the same */
	assert((void *)x == (void *)y);

	Singleton *z = Singleton::make_object(1,2);

	/** Underlying Singleton pointers are all the same */
	assert((void *)x == (void *)y && (void *)y == (void *)z);

	assert(x->a == 1 && y->a == 1 && z->a == 1);
	assert(x->b == 2 && y->b == 2 && z->b == 2);

	x->a = 10;
	y->b = 20;

	assert(x->a == 10 && y->a == 10 && z->a == 10);
	assert(x->b == 20 && y->b == 20 && z->b == 20);

	//x->print();
	//y->print();
	//z->print();
	
	return 0;
}

