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
