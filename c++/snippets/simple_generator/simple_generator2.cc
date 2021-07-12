#include <iostream>

// Generator using a lambda

int main() {
	auto sg = [i = 0]() mutable {
		return i++;
	};
	
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
