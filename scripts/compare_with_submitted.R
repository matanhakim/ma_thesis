# Compare the rendered thesis with the version submitted in 2024.
#
# The submitted thesis is the yardstick of this project: the rendered files
# should carry the same text, figures and tables. Two comparisons are made.
#
# 1. Word against Word. The paragraphs of output/thesis.docx are compared with
#    those of reference/thesis_submitted_2024.docx. Both hold the text in
#    logical order, so this is the cleanest measure of textual identity.
# 2. PDF against PDF. The text of output/thesis.pdf (Typst) is compared with
#    the text of reference/thesis_submitted_2024.pdf. Text extracted from a
#    right-to-left PDF depends on the renderer (line breaks, hyphenation,
#    digits attached to words), so only sequences of letters are compared.
#
# For each comparison the report gives the Sørensen–Dice similarity of the two
# word multisets (2 * shared words / total words), the share of the submitted
# words found in the rendered text, and, for Word, the share of submitted
# paragraphs of ten words or more that appear verbatim.
#
# Usage: Rscript scripts/compare_with_submitted.R [report.md]
# Default report: output/comparison_report.md. Exits with an error when the
# Word comparison falls below 98%.

args <- commandArgs(trailingOnly = TRUE)
report_path <- if (length(args) >= 1) args[[1]] else file.path("output", "comparison_report.md")

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
})

submitted_docx <- file.path("reference", "thesis_submitted_2024.docx")
submitted_pdf <- file.path("reference", "thesis_submitted_2024.pdf")
rendered_docx <- file.path("output", "thesis.docx")
rendered_pdf <- file.path("output", "thesis.pdf")

# ---- helpers -----------------------------------------------------------------

# Words are runs of Hebrew or Latin letters; digits, punctuation and one-letter
# tokens are dropped.
tokenise <- function(text) {
  str_extract_all(text, "[A-Za-z\\p{Hebrew}]+")[[1]] |>
    keep(\(w) nchar(w) > 1)
}

shared_count <- function(a, b) {
  ta <- table(a)
  tb <- table(b)
  common <- intersect(names(ta), names(tb))
  sum(pmin(ta[common], tb[common]))
}

dice <- function(a, b) 2 * shared_count(a, b) / (length(a) + length(b))

normalise <- function(text) {
  text |>
    str_replace_all("[^A-Za-z\\p{Hebrew}]+", " ") |>
    str_squish()
}

# Paragraph texts of a Word document, in logical order. Field instructions
# (TOC, PAGEREF, SEQ ...) and the generated table-of-contents and list-of-
# figures paragraphs are left out of both documents: they are produced by Word
# itself, not written by the author.
docx_paragraphs <- function(path) {
  tmp <- tempfile()
  zip::unzip(path, files = "word/document.xml", exdir = tmp)
  doc <- xml2::read_xml(file.path(tmp, "word", "document.xml"))
  ns <- xml2::xml_ns(doc)
  xml2::xml_remove(xml2::xml_find_all(doc, ".//w:instrText | .//w:delText", ns))
  paragraphs <- xml2::xml_find_all(doc, ".//w:body//w:p", ns)
  style <- purrr::map_chr(paragraphs, \(p) {
    s <- xml2::xml_find_first(p, "./w:pPr/w:pStyle", ns)
    if (inherits(s, "xml_missing")) "" else xml2::xml_attr(s, "val")
  })
  text <- purrr::map_chr(paragraphs, \(p) {
    paste(xml2::xml_text(xml2::xml_find_all(p, ".//w:t", ns)), collapse = "")
  })
  generated <- str_detect(tolower(style), "^toc|tableoffigures|table of figures|^tof")
  text[!generated & text != ""]
}

pdf_text <- function(path) paste(pdftools::pdf_text(path), collapse = "\n")

# ---- 1. Word against Word ---------------------------------------------------

sub_paras <- docx_paragraphs(submitted_docx)
ren_paras <- docx_paragraphs(rendered_docx)
sub_words <- tokenise(paste(sub_paras, collapse = " "))
ren_words <- tokenise(paste(ren_paras, collapse = " "))
docx_dice <- dice(sub_words, ren_words)
docx_recall <- shared_count(sub_words, ren_words) / length(sub_words)

ren_norm <- normalise(paste(ren_paras, collapse = " "))
long_paras <- sub_paras |> normalise() |> keep(\(p) str_count(p, "\\S+") >= 10)
found <- map_lgl(long_paras, \(p) str_detect(ren_norm, fixed(p)))

# Submitted words missing from the rendered text, for inspection.
missing_words <- {
  ta <- table(sub_words)
  tb <- table(ren_words)
  diff <- ta - ifelse(is.na(tb[names(ta)]), 0, tb[names(ta)])
  diff <- diff[diff > 0]
  head(sort(diff, decreasing = TRUE), 15)
}

# ---- 2. PDF against PDF -----------------------------------------------------

sub_pdf_words <- tokenise(pdf_text(submitted_pdf))
ren_pdf_words <- tokenise(pdf_text(rendered_pdf))
pdf_dice <- dice(sub_pdf_words, ren_pdf_words)
pdf_recall <- shared_count(sub_pdf_words, ren_pdf_words) / length(sub_pdf_words)

# ---- report -------------------------------------------------------------------

pct <- function(x) sprintf("%.2f%%", 100 * x)
report <- c(
  "# Comparison of the rendered thesis with the submitted version",
  "",
  sprintf("Generated on %s.", format(Sys.Date())),
  "",
  "## Word document (logical text, paragraph by paragraph)",
  "",
  sprintf("* Submitted: `%s`, %d words", submitted_docx, length(sub_words)),
  sprintf("* Rendered: `%s`, %d words", rendered_docx, length(ren_words)),
  sprintf("* Word similarity (Sørensen–Dice): **%s**", pct(docx_dice)),
  sprintf("* Submitted words present in the rendered document: %s", pct(docx_recall)),
  sprintf("* Submitted paragraphs of ten words or more found verbatim: **%s** (%d of %d)",
          pct(mean(found)), sum(found), length(found)),
  "",
  "Most frequent submitted words with fewer occurrences in the rendered document",
  "(Word fields and the generated contents and lists are excluded on both sides;",
  "the text drawn inside Figure 3 of the submitted thesis is an image here):",
  "",
  sprintf("* %s (%d)", names(missing_words), as.integer(missing_words)),
  "",
  "## PDF (text extracted with poppler, letters only)",
  "",
  sprintf("* Submitted: `%s`, %d words", submitted_pdf, length(sub_pdf_words)),
  sprintf("* Rendered: `%s`, %d words", rendered_pdf, length(ren_pdf_words)),
  sprintf("* Word similarity (Sørensen–Dice): **%s**", pct(pdf_dice)),
  sprintf("* Submitted words present in the rendered PDF: %s", pct(pdf_recall)),
  "",
  "## Submitted paragraphs not found verbatim in the Word document",
  "",
  if (any(!found)) paste0("* ", str_trunc(long_paras[!found], 140)) else "None."
)
writeLines(report, report_path, useBytes = TRUE)
cat(report, sep = "\n")

if (docx_dice < 0.98) {
  stop(sprintf("Word similarity %s is below the 98%% target", pct(docx_dice)))
}
