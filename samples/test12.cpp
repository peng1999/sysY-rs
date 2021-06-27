// in: 0
// in: 5
int getchar();

int main() {
    int zero = 48;
    int a = getchar() - zero;
    if (a < 3) {
        a = 1;
        return a + zero;
    } else {
        return a - 1;
    }
}
