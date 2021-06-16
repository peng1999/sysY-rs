int putchar(int a);

void many(int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8, int a9) {
    putchar(a9);
}

int second(int y, int x) {
    return x;
}

int main() {
    many(0, 1, 2, 3, 4, 5, 6, 7, 49);
    int a = 1;
    int b = second(2, a);
    return b;
}
