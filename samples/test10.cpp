int fun(int x) {
    return x;
}
int main() {
    int a = 1;
    int b = a + a * 2;
    int c = a * (b + 2 * a);
    a = c + fun(a) - 1;
    c = 2 + b / 2;
    return a;
}
