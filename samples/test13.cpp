int main() {
    auto a = 1;
    auto b = a + 3;
    auto c = a == b;
    if (c) {
        a = 4;
    }
    return b;
}
