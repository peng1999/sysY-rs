int b2i(bool v) {
    if (v) {
        return 1;
    } else {
        return 0;
    }
}

bool fun(int a, int b) {
    auto c = a == 4 || b < 4;
    return c;
}
int main() {
    auto a = 1;
    auto b = a + 3;
    return b2i(fun(a, b));
}
