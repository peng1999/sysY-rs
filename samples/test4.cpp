// in: 3
// in: 5
int getchar();
int putchar(int a);

int fib(int n) {
    if (n <= 1) return 1;
    return fib(n - 1) + fib(n - 2);
}

int main() {
    int i = getchar() - 48;
    int sum = fib(i);
    putchar(sum + 48);
    return 0;
}
