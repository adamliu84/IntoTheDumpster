#ifdef __cplusplus
extern "C" {
#endif

struct Point {
    int x;
    int y;
};

int add_cpp_numbers(int a, int b);
int multiply_cpp_numbers(int a, int b);
void log_cpp(const char* message);
struct Point* update_cpp_point(struct Point* p);

#ifdef __cplusplus
}
#endif