#include <iostream>

class SimpleGenerator {
	int cnt = 0;
public:
	int operator()() {
		return cnt++;
	}
};


int main() {
	SimpleGenerator sg;
	std::cout << sg() << std::endl;
	std::cout << sg() << std::endl;
	std::cout << sg() << std::endl;
	std::cout << sg() << std::endl;
}

// Outputs:
// 0
// 1
// 2
// 3
