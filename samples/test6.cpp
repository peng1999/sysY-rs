// in: 0
// in: 1
int getchar();
int main() {
    int a;
    if (getchar() == 48) {// '0'
        a = 42;
    } else {
        a = 0;
    }
    return a;
}
