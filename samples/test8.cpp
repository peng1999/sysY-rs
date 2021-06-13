// in: 00
// in: 01
// in: 10
// in: 11
int getchar();
int putchar(int a);

int main() {
    int zeroc = 48;
    int a = getchar() - zeroc;
    int b = getchar() - zeroc;
    int arr[2][2];
    int i = 0;
    while (i < 2) {
        int j = 0;
        while (j < 2) {
            arr[i][j] = i * 2 + j;
            j = j + 1;
        }
        i = i + 1;
    }
    putchar(arr[a][b] + zeroc);
    return 0;
}
