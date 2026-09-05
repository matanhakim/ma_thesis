# Post-render fixes for the Word output (run by Quarto after rendering).
#
# 1. Ask Word to refresh its fields when the document is opened, so the table
#    of contents and the lists of figures and tables (inserted as TOC fields
#    in thesis.qmd) are filled in with page numbers.
# 2. Make bold and italic runs bold and italic in Hebrew as well: Word keeps
#    separate "complex script" flags (w:bCs, w:iCs) that Pandoc does not set.
#
# The script edits the XML parts of the .docx (a zip archive) in place.

docx_path <- file.path("output", "thesis.docx")
rendered <- Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES", unset = docx_path)
if (!file.exists(docx_path) || !grepl("thesis.docx", rendered, fixed = TRUE)) {
  quit(save = "no", status = 0)
}

tmp <- tempfile("docx")
dir.create(tmp)
zip::unzip(docx_path, exdir = tmp)

settings_path <- file.path(tmp, "word", "settings.xml")
settings <- readLines(settings_path, encoding = "UTF-8", warn = FALSE)
settings <- paste(settings, collapse = "\n")
if (!grepl("w:updateFields", settings, fixed = TRUE)) {
  settings <- sub(
    "(<w:settings[^>]*>)",
    "\\1<w:updateFields w:val=\"true\"/>",
    settings
  )
}
writeLines(settings, settings_path, useBytes = TRUE)

document_path <- file.path(tmp, "word", "document.xml")
document <- paste(readLines(document_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
document <- gsub("<w:b ?/>(?!<w:bCs)", "<w:b/><w:bCs/>", document, perl = TRUE)
document <- gsub("<w:i ?/>(?!<w:iCs)", "<w:i/><w:iCs/>", document, perl = TRUE)
# 3. Chapter numbers without the trailing period Pandoc adds ("1. מבוא" -> "1 מבוא"),
#    matching the numbering of the submitted thesis.
document <- gsub(
  "(<w:pStyle w:val=\"Heading1\" ?/>.*?<w:t xml:space=\"preserve\">)(\\d+)\\. ",
  "\\1\\2 ", document, perl = TRUE
)
# 4. Let Word size table columns from their content instead of the fixed
#    widths Pandoc derives from the Markdown grid, which squeeze the numeric
#    columns of the regression table.
document <- gsub("<w:tblW [^>]*/>", "<w:tblW w:w=\"0\" w:type=\"auto\"/>", document, perl = TRUE)
document <- gsub("<w:tcW [^>]*/>", "<w:tcW w:w=\"0\" w:type=\"auto\"/>", document, perl = TRUE)
document <- gsub("<w:tblLayout [^>]*/>", "", document, perl = TRUE)
document <- gsub("(<w:tblPr>)", "\\1<w:tblLayout w:type=\"autofit\"/>", document, perl = TRUE)
# 5. Data tables (Pandoc's "Table" style): cell paragraphs without the body
#    text's first-line indent, 10 pt text as in the submitted thesis, and the
#    regression table (Table 4, header "Characteristic") reading left to right
#    as in the submitted thesis.
format_table <- function(tbl) {
  # Quarto wraps figures in single-cell tables too; leave those alone.
  if (!grepl("<w:tblStyle w:val=\"Table\"", tbl, fixed = TRUE) || grepl("<w:drawing>", tbl, fixed = TRUE)) {
    return(tbl)
  }
  no_indent <- "<w:ind w:firstLine=\"0\"/>"
  tbl <- gsub("<w:p>(?!<w:pPr>)", paste0("<w:p><w:pPr>", no_indent, "</w:pPr>"), tbl, perl = TRUE)
  tbl <- gsub("<w:p><w:pPr>(?!<w:ind)", paste0("<w:p><w:pPr>", no_indent), tbl, perl = TRUE)
  size <- "<w:sz w:val=\"20\"/><w:szCs w:val=\"20\"/>"
  tbl <- gsub("<w:rPr>(?!<w:sz)", paste0("<w:rPr>", size), tbl, perl = TRUE)
  tbl <- gsub("<w:r>(?!<w:rPr>)", paste0("<w:r><w:rPr>", size, "</w:rPr>"), tbl, perl = TRUE)
  if (grepl("Characteristic", tbl, fixed = TRUE)) {
    tbl <- gsub("<w:bidiVisual ?/>", "", tbl, perl = TRUE)
    tbl <- gsub("<w:bidi ?/>", "", tbl, perl = TRUE) # cell paragraphs left to right as well
    tbl <- gsub("<w:rtl ?/>", "", tbl, perl = TRUE) # so that "-23,734" keeps its sign in front
  }
  tbl
}
tables <- gregexpr("<w:tbl>.*?</w:tbl>", document, perl = TRUE)[[1]]
if (tables[1] > 0) {
  for (k in rev(seq_along(tables))) {
    start <- tables[k]
    end <- start + attr(tables, "match.length")[k] - 1
    tbl <- format_table(substr(document, start, end))
    document <- paste0(substr(document, 1, start - 1), tbl, substr(document, end + 1, nchar(document)))
  }
}
writeLines(document, document_path, useBytes = TRUE)

# Re-zip with the directory structure intact ("mirror" mode keeps the paths
# relative to `tmp`); Word expects [Content_Types].xml as the first entry.
files <- list.files(tmp, recursive = TRUE, all.files = TRUE)
files <- c("[Content_Types].xml", setdiff(files, "[Content_Types].xml"))
docx_abs <- normalizePath(docx_path, mustWork = TRUE)
unlink(docx_abs)
zip::zip(docx_abs, files = files, root = tmp, mode = "mirror")
unlink(tmp, recursive = TRUE)
message("post-processed ", docx_path)
