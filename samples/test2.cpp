int main() {
    int sum = 0;
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
