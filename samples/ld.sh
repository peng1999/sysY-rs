#!/usr/bin/sh
# input: /tmp/main.o
# output: /tmp/a.out
ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o /tmp/a.out /lib64/Scrt1.o /lib64/crti.o -L/usr/lib -L/lib/gcc/x86_64-pc-linux-gnu/11.1.0/ /tmp/main.o -lc -lgcc /lib/crtn.o
