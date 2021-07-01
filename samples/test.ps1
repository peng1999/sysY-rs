#!/usr/bin/pwsh

function err($msg) {
    $host.ui.WriteErrorLine($msg)
}

if ($args) {
    $cpps = $args | Get-ChildItem
} else {
    $cpps = Get-ChildItem $PSScriptRoot -Filter "test*.cpp"
}

$lib = Join-Path $PSScriptRoot "sysy.a"
$exe = "/tmp/main"
$flags = "-fgcp", "-fdce"

$projectMeta = cargo metadata --format-version 1 | ConvertFrom-Json
$targetRoot = $projectMeta.target_directory
Write-Output "Project target: $targetRoot"
cargo build
if (-not $?) {
    err "[cargo build] Error"
    return
}

make -C samples

$compiler = Join-Path $targetRoot "debug/sysy-rs"
$myobj = "/tmp/main.o"
$myexe = "/tmp/a.out"
$myasm = "/tmp/main.s"
$riscvexe = "/tmp/main1"

$total = 0
$passed = 0
$riscvpassed = 0

foreach ($cpp in $cpps) {
    Write-Output "`n=== $($cpp.Name) ==="
    # standard compiler
    gcc -o $exe $cpp $lib
    # llvm backend
    & $compiler -o $myobj $flags $cpp &&
        gcc -o $myexe $myobj
    if (-not $?) {
        err "Compile error"
        $total += 1
        continue
    }
    # riscv32 backend
    $riscvpass = $True
    & $compiler -o $myasm --emit=riscv $flags $cpp &&
        pwsh samples/riscv32-elf-gcc.ps1 $myasm "main1"
    if (-not $?) {
        err "RISC-V Compile error"
        $riscvpass = $False
    }
    $content = Get-Content $cpp
    for ($i = 0; $i -lt $content.Count; $i++) {
        $in = ""
        if ($content[$i] -match "^// in: ") {
            $in = $content[$i].Substring(7)
        } elseif ($i -gt 0) {
            break
        }
        $total += 1

        $truth = $in | & $exe | Out-String
        $status = $LASTEXITCODE

        $out = $in | & $myexe | Out-String
        $mystatus = $LASTEXITCODE

        $riscvdiff = $False
        if ($riscvpass) {
            $riscvout = $in | qemu-riscv32 $riscvexe | Out-String
            $riscvstatus = $LASTEXITCODE
            $riscvdiff = ($riscvout -ne $truth) -or ($riscvstatus -ne $status)
        }

        Write-Output "stdin: '$in' stdout: '$truth' = $status"
        if (($out -ne $truth) -or ($mystatus -ne $status)) {
            err "sysy-rs: '$out' = $mystatus"
        } else {
            $passed += 1
        }
        if ($riscvpass) {
            if ($riscvdiff) {
                err "riscv-back: '$riscvout' = $riscvstatus"
            } else {
                $riscvpassed += 1
            }
        }
    }
}

Write-Output "`n==========="
Write-Output "Total tests: $total"
Write-Output "LLVM passed: $passed"
Write-Output "RISC-V passed: $riscvpassed"
