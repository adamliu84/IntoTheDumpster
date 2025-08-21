#include "cpp_library.h"
#include <iostream>
#include <string.h>

void internal_log(const std::string& message){
    std::cout << "From C++: " << message << std::endl;
}

extern "C" {
    int add_cpp_numbers(int a, int b) {    
        return a + b;
    }

    int multiply_cpp_numbers(int a, int b) {    
        return a * b;
    }

    void log_cpp(const char* message) {    
        internal_log(message);    
    }

    struct Point* update_cpp_point(struct Point* p) {        
        p->x = p->x * 2;
        p->y = p->y + 10;
        std::cout << "From C++: Point updated to (" << p->x << ", " << p->y << ")" << std::endl;        
        return p;
    }
}