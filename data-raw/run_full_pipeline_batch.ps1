param(
  [int]$Runs = 5
)

$projectRoot = Split-Path -Parent $PSScriptRoot
$rscriptPath = 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe'

Set-Location $projectRoot
& $rscriptPath (Join-Path $projectRoot 'data-raw\run_full_pipeline_batch.R') $Runs
exit $LASTEXITCODE