int putchar(int c);

void ping(int n);
void pong(int n);

int main() {
    ping(5);
    return 0;
}

void ping(int n) {
    if (n > 0) {
        putchar(105);
        pong(n - 1);
    }
}

void pong(int n) {
    if (n > 0) {
        putchar(111);
        ping(n - 1);
    }
}
