// in: 4312
// in: 3142
int getchar();
int putchar(int a);

int main() {
    int zeroc = 48;
    int a[4];
    int i = 0;
    while (i < 4) {
        a[i] = getchar() - zeroc;
        i = i + 1;
    }

    i = 0;
    while (i < 3) {
        int j = i + 1;
        while (j < 4) {
            if (a[i] > a[j]) {
                int ai = a[i];
                int aj = a[j];
                a[i] = aj;
                a[j] = ai;
            }
            j = j + 1;
        }
        i = i + 1;
    }

    i = 0;
    while (i < 4) {
        putchar(a[i] + zeroc);
        i = i + 1;
    }
    return 0;
}
