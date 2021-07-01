// in: 0
// in: 5
int getchar();

int main() {
    int zero = 48;
    int a = getchar() - zero;
    int b = a + 1;
    if (a < 3) {
        b = a + 2;
        a = 1;
        return a + zero;
    } else {
        b = a + 2;
        return a - b;
    }
}
