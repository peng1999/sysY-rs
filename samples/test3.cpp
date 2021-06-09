// in: 1
// in: 5
int getchar();
int putchar(int a);

int main() {
    int sum = getchar();
    int i = 1;
    while (i <= 10) {
        sum = sum + i;
        i = i + 1;
    }
    if (i != 11) {
        //sum = -1;
        sum = 42;
    }
    return sum;
}
