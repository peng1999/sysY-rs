#!/bin/pwsh
param ($file, $output)

if ((-not $file) -or (-not $output)) {
    exit 1
}

$file = Get-ChildItem $file
$dir = $file.Directory
$name = $file.Name
$basename = $file.BaseName

docker run --rm -it -v ${dir}:/123 --workdir /123 0x01be/riscv-gnu-toolchain:rv32gc riscv32-unknown-elf-gcc -o $output $name
