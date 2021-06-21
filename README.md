# 一个 Rust 编译器实现

## 语法

实现了C++语法的一个子集，包括

- 函数
- 函数声明
- 数组
- `int` `bool` 变量
- `if` `while` 控制流

函数默认为外部链接性，不支持重载，没有mangle。

## 示例

输入文件 `code.cpp`

```cpp
int putchar(int a);

int main() {
    putchar(48);
    putchar(49);
    return 0;
}
```

利用LLVM后端进行编译：

```console
$ # 编译
$ cargo run -- -o code.o code.cpp
$ # 链接
$ gcc -o main code.o
$ ./main
01
```

利用RISC-V后端进行编译：

```console
$ # 生成 RV32 汇编
$ cargo run -- --emit=riscv -o code.s code.cpp
$ # 编译链接
$ riscv32-unknown-elf-gcc -o main code.s

$ qemu-riscv32 ./main
01
```
