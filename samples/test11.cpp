// in: 11
// in: 00
// in: 24
// in: 02
// in: 23
int getchar();

int main() {
    int a = getchar() - 48;
    int b = getchar() - 48;
    if (a == 1 || b < 1) {
        return 2;
    }
    if (2 == a && 3 < b) {
        return 3;
    }

    return 0;
}
