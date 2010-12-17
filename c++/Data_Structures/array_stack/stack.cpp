#include <iostream>
using namespace std;

#define MAX_SIZE 100

template <class T>
class stack {
	private:
		T values[MAX_SIZE];
		int tos;
	
	public:
		stack (void) {
			tos = -1;
		}
			
		bool push (T data) {
			if (tos == MAX_SIZE-1)
				return false;
			
			values[++tos] = data;
			return true;
		}

		bool pop(T &data) {
			if (tos == -1)
				return false;

			data = values[tos--];
		}
		void display (void) {
			if (tos == -1) {
				cout<<"Stack Empty!!"<<endl;
				return;
			}

			cout<<"["<<(tos+1)<<"]: ";
			for (int i=0; i<=tos; i++) {
				cout<<values[i]<<" ";
			}
			cout<<endl;
		}
};

int main(void)
{
	stack<int> intstack;
	stack<float> fltstack;

	intstack.push(1);
	intstack.push(2);

	fltstack.push(3.01);
	fltstack.push(4.02);

	intstack.display();
	fltstack.display();

	return 0;
}
