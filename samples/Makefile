.PHONY: all clean

all: sysy.a

wrap.o: wrap.cpp
	gcc -c wrap.cpp

lib.o: lib.cpp
	gcc -c lib.cpp

sysy.a: lib.o wrap.o
	ar rcs sysy.a lib.o wrap.o

clean:
	rm wrap.o lib.o sysy.a
