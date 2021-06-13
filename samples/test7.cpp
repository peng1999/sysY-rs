int main() {
    int a[3];
    a[0] = 1;
    a[1] = 2;
    a[a[1]] = 1 + a[0];
    return a[2];
}
