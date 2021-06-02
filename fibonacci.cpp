#include <iostream>

unsigned long long fib(unsigned long long n, unsigned long long p1, unsigned long long p2) {
    if (n == 0) return p2;
    std::cout << p2 << std::endl;
    return fib(n - 1, p2, p1 + p2);
}


int main() {
    int pos = 0;
    std::cout << "Enter position on Fibonacci sequence you want to calculate(up to 93rd pos): ";
    std::cin >> pos;
    std::cout << "The number is: " << fib(pos - 1, 0, 1);
    return 0;
}
