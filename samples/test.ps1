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

foreach ($cpp in $cpps) {
    clang++ -o $exe $cpp $lib
    & $compiler -o $myobj $cpp
    if (-not $?) {
        Write-Error "[$($cpp.Name)] Compile error"
        continue
    }
    clang -o $myexe $myobj
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
        if (($out -ne $truth) -or ($mystatus -ne $status)) {
            Write-Error "[$($cpp.Name)] Opps! stdin: '$in'"
            Write-Error "clang: '$truth' =$status"
            Write-Error "sysy-rs: '$mytruth' =$mystatus"
        } else {
            Write-Output "[$($cpp.Name)] stdin: '$in' stdout: '$truth' = $status"
        }
    }
}
