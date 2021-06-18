#!/usr/bin/pwsh

$cpps = Get-ChildItem $PSScriptRoot -Filter "test*.cpp"
$lib = Join-Path $PSScriptRoot "sysy.a"
$exe = "/tmp/main"

$projectMeta = cargo metadata --format-version 1 | ConvertFrom-Json
$targetRoot = $projectMeta.target_directory
Write-Output "Project target: $targetRoot"
cargo build
if (-not $?) {
    Write-Error "[cargo build] Error"
    return
}

make -C samples

$compiler = Join-Path $targetRoot "debug/sysy-rs"
$myobj = "/tmp/main.o"
$myexe = "/tmp/a.out"
$myasm = "/tmp/main.s"
$riscvexe = "/tmp/main1"

foreach ($cpp in $cpps) {
    Write-Output "`n=== $($cpp.Name) ==="
    # standard compiler
    gcc -o $exe $cpp $lib
    # llvm backend
    & $compiler -o $myobj $cpp &&
        gcc -o $myexe $myobj
    if (-not $?) {
        Write-Error "[$($cpp.Name)] Compile error"
        continue
    }
    # riscv32 backend
    $riscvpass = $True
    & $compiler -o $myasm --emit=riscv $cpp &&
        pwsh samples/riscv32-elf-gcc.ps1 $myasm "main1"
    if (-not $?) {
        Write-Error "[$($cpp.Name)] RiscV Compile error"
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
        $truth = $in | & $exe
        $status = $LASTEXITCODE

        $out = $in | & $myexe
        $mystatus = $LASTEXITCODE

        $riscvdiff = $False
        if ($riscvpass) {
            $riscvout = $in | qemu-riscv32 $riscvexe
            $riscvstatus = $LASTEXITCODE
            $riscvdiff = ($riscvout -ne $truth) -or ($riscvstatus -ne $status)
        }

        Write-Output "[$($cpp.Name)] stdin: '$in' stdout: '$truth' = $status"
        if (($out -ne $truth) -or ($mystatus -ne $status)) {
            Write-Error "sysy-rs: '$out' =$mystatus"
        }
        if ($riscvdiff -and $riscvpass) {
            Write-Error "riscv-back: '$riscvout' =$riscvstatus"
        }
    }
}
