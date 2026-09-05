# Open the rendered Word document in Microsoft Word, refresh its fields (table
# of contents, lists of figures and tables, page references) and export it to
# PDF. Windows with Word installed only; used to inspect the Word rendering
# and to compare it with the submitted thesis.
#
#   powershell -File scripts/word_to_pdf.ps1 [-In output\thesis.docx] [-Out output\thesis_word.pdf]
param(
  [string]$In = "output\thesis.docx",
  [string]$Out = "output\thesis_word.pdf"
)
$ErrorActionPreference = "Stop"
$In = (Resolve-Path $In).Path
$Out = Join-Path (Get-Location) $Out

$word = New-Object -ComObject Word.Application
$word.Visible = $false
$word.DisplayAlerts = 0
try {
  $doc = $word.Documents.Open($In)
  $doc.Fields.Update() | Out-Null
  foreach ($toc in $doc.TablesOfContents) { $toc.Update() }
  foreach ($tof in $doc.TablesOfFigures) { $tof.Update() }
  $doc.Repaginate()
  $doc.ExportAsFixedFormat($Out, 17)   # 17 = wdExportFormatPDF
  $doc.Close($false)
  Write-Output "wrote $Out"
} finally {
  $word.Quit()
  [System.Runtime.InteropServices.Marshal]::ReleaseComObject($word) | Out-Null
}
