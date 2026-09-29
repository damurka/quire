# Printing and converting: the report as a PDF and as pictures of its pages, by what the computer has -- Microsoft Word
# or PowerPoint (Windows), LibreOffice, or a Chromium browser (Chrome, Edge, Chromium) for the builder's printable page.
# Nothing here is an app's: every host that writes the builder's files can use it.

#' The program that turns a Word or PowerPoint file into a PDF
#'
#' Microsoft Word or PowerPoint (Windows), else LibreOffice. With one of them the PDF has exactly the file's pages.
#' @param kind `"document"` (a Word file) or `"deck"` (a PowerPoint file).
#' @return `"word"` or `"powerpoint"`, `"libreoffice"`, or `NULL` when none is installed.
#' @export
quire_converter <- function(kind = c("document", "deck")) {
  kind <- match.arg(kind)
  office <- if (kind == "deck") "powerpoint" else "word"
  if (.Platform$OS.type == "windows") {
    key <- if (kind == "deck") "PowerPoint.Application\\CurVer" else "Word.Application\\CurVer"
    found <- tryCatch(length(utils::readRegistry(key, "HCR")) > 0, error = function(e) FALSE)
    if (isTRUE(found)) return(office)
  }
  if (nzchar(.quire_soffice())) return("libreoffice")
  NULL
}

.quire_soffice <- function() {
  candidates <- c(
    Sys.which(c("soffice", "libreoffice")),
    file.path(Sys.getenv("PROGRAMFILES"), "LibreOffice/program/soffice.exe"),
    file.path(Sys.getenv("PROGRAMFILES(X86)"), "LibreOffice/program/soffice.exe"),
    "/Applications/LibreOffice.app/Contents/MacOS/soffice"
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit)) hit[[1]] else ""
}

# A PowerShell script run once, with the file in and the file out
.quire_powershell <- function(lines, input, output = NULL, what) {
  script <- tempfile(fileext = ".ps1")
  on.exit(unlink(script), add = TRUE)
  writeLines(c("param([string]$In, [string]$Out)", "$ErrorActionPreference = 'Stop'", lines), script)
  args <- c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(normalizePath(script, winslash = "\\")),
            "-In", shQuote(normalizePath(input, winslash = "\\")))
  if (!is.null(output)) args <- c(args, "-Out", shQuote(normalizePath(output, winslash = "\\", mustWork = FALSE)))
  out <- suppressWarnings(system2("powershell", args, stdout = TRUE, stderr = TRUE, timeout = 300))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0
  if (!identical(as.integer(status), 0L) || (!is.null(output) && !file.exists(output))) {
    stop(sprintf("%s could not make the file. %s", what, paste(utils::tail(out, 3), collapse = " ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Finish a Word file in Word
#'
#' Microsoft Word opens the file, fills in its contents page and page numbers, embeds its fonts and saves it; with
#' `pdf`, it also saves the PDF, so the Word file and the PDF are the same document. Windows, with Word installed.
#' @param docx The Word file.
#' @param pdf The PDF to write too, or `NULL`.
#' @return `TRUE`, invisibly; an error when Word could not.
#' @export
quire_word_finish <- function(docx, pdf = NULL) {
  .quire_powershell(c(
    "$word = New-Object -ComObject Word.Application",
    "$word.Visible = $false",
    "$word.DisplayAlerts = 0",
    "try {",
    "  $doc = $word.Documents.Open($In, $false, $false, $false)",
    "  foreach ($t in $doc.TablesOfContents) { $t.Update() }",
    "  $doc.Fields.Update() | Out-Null",
    "  $doc.EmbedTrueTypeFonts = $true",
    "  $doc.SaveSubsetFonts = $true",
    "  $doc.Save()",
    "  if ($Out) { $doc.ExportAsFixedFormat($Out, 17) }",
    "  $doc.Close($false)",
    "} finally { $word.Quit() }"
  ), docx, pdf, "Microsoft Word")
}

#' A Word or PowerPoint file as a PDF
#'
#' @param file The Word (`.docx`) or PowerPoint (`.pptx`) file.
#' @param pdf The PDF to write.
#' @param converter `"word"`, `"powerpoint"` or `"libreoffice"`; by default what [quire_converter()] finds.
#' @return `pdf`, invisibly, with attribute `converter`.
#' @export
quire_to_pdf <- function(file, pdf, converter = NULL) {
  deck <- grepl("\\.pptx$", file, ignore.case = TRUE)
  converter <- converter %||% quire_converter(if (deck) "deck" else "document")
  if (is.null(converter)) {
    stop(if (deck) "Making a PDF of slides needs Microsoft PowerPoint or LibreOffice." else "Making a PDF of a Word file needs Microsoft Word or LibreOffice.", call. = FALSE)
  }
  if (converter == "word") {
    quire_word_finish(file, pdf)
  } else if (converter == "powerpoint") {
    # PowerPoint runs once per computer: closed afterwards only when no other presentation is open in it
    .quire_powershell(c(
      "$pp = New-Object -ComObject PowerPoint.Application",
      "try {",
      "  $pres = $pp.Presentations.Open($In, -1, 0, 0)",
      "  $pres.SaveAs($Out, 32)",
      "  $pres.Close()",
      "} finally { if ($pp.Presentations.Count -eq 0) { $pp.Quit() } }"
    ), file, pdf, "Microsoft PowerPoint")
  } else {
    outdir <- tempfile("lo_")
    dir.create(outdir)
    on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
    out <- suppressWarnings(system2(.quire_soffice(), c("--headless", "--convert-to", "pdf", "--outdir", shQuote(outdir), shQuote(file)),
                                    stdout = TRUE, stderr = TRUE, timeout = 300))
    made <- file.path(outdir, sub("\\.[^.]*$", ".pdf", basename(file)))
    if (!file.exists(made)) stop(sprintf("LibreOffice could not make the PDF. %s", paste(utils::tail(out, 3), collapse = " ")), call. = FALSE)
    file.copy(made, pdf, overwrite = TRUE)
  }
  invisible(structure(pdf, converter = converter))
}

# chromote looks for Google Chrome; on a machine without it, another Chromium browser (Edge is on every Windows
# machine). FALSE when there is none.
.quire_find_browser <- function() {
  if (!requireNamespace("chromote", quietly = TRUE)) return(FALSE)
  if (nzchar(Sys.getenv("CHROMOTE_CHROME"))) return(TRUE)
  found <- tryCatch(suppressMessages(nzchar(chromote::find_chrome() %||% "")), error = function(e) FALSE)
  if (isTRUE(found)) return(TRUE)
  candidates <- c(
    file.path(Sys.getenv("PROGRAMFILES"), "Google/Chrome/Application/chrome.exe"),
    file.path(Sys.getenv("PROGRAMFILES(X86)"), "Google/Chrome/Application/chrome.exe"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe"),
    file.path(Sys.getenv("PROGRAMFILES(X86)"), "Microsoft/Edge/Application/msedge.exe"),
    file.path(Sys.getenv("PROGRAMFILES"), "Microsoft/Edge/Application/msedge.exe"),
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    Sys.which(c("chromium", "chromium-browser", "google-chrome", "microsoft-edge"))
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(hit)) return(FALSE)
  Sys.setenv(CHROMOTE_CHROME = hit[[1]])
  TRUE
}

#' Whether this computer can print the builder's printable page
#'
#' @return `TRUE` when the chromote package and a Chromium browser (Chrome, Edge, Chromium) are there: then
#'   [quire_html_pdf()] and [quire_print_pages()] work.
#' @export
quire_can_print <- function() isTRUE(.quire_find_browser())

#' The builder's printable page as a PDF
#'
#' Prints the page (the HTML the builder makes a PDF from) with a Chromium browser, its page size, margins, headers and
#' footers as the page sets them (CSS `@page`). Needs the chromote package and Chrome, Edge or Chromium.
#' @param html The printable page: its HTML, or an `.html` file.
#' @param pdf The PDF to write.
#' @return `pdf`, invisibly.
#' @export
quire_html_pdf <- function(html, pdf) {
  if (!quire_can_print()) stop("Printing the page needs the chromote package and Google Chrome, Microsoft Edge or Chromium.", call. = FALSE)
  page <- html
  if (!(length(html) == 1 && nchar(html) < 2000 && !grepl("<", html, fixed = TRUE) && file.exists(html))) {
    page <- tempfile(fileext = ".html")
    on.exit(unlink(page), add = TRUE)
    writeBin(charToRaw(enc2utf8(paste(html, collapse = "\n"))), page)
  }
  b <- chromote::ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(paste0("file:///", normalizePath(page, winslash = "/")), wait_ = FALSE)
  b$wait_for(loaded)
  out <- b$Page$printToPDF(printBackground = TRUE, preferCSSPageSize = TRUE)
  writeBin(jsonlite::base64_dec(out$data), pdf)
  invisible(pdf)
}

#' The pages of the builder's printable page, as pictures
#'
#' Prints the page to PDF ([quire_html_pdf()]) and renders each page as a picture: what the builder's Print Preview
#' shows. Needs the pdftools package too.
#' @inheritParams quire_html_pdf
#' @param dpi Resolution of the pictures.
#' @return `list(pages = <list of PNG data URLs>, converter = "browser")`.
#' @export
quire_print_pages <- function(html, dpi = 80) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("Print Preview needs the pdftools package.", call. = FALSE)
  dir <- tempfile("print_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  pdf <- file.path(dir, "report.pdf")
  quire_html_pdf(html, pdf)
  n <- pdftools::pdf_info(pdf)$pages
  files <- file.path(dir, sprintf("page_%03d.png", seq_len(n)))
  pdftools::pdf_convert(pdf, format = "png", dpi = dpi, filenames = files, verbose = FALSE)
  pages <- lapply(files, function(f) paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size))))
  list(pages = pages, converter = "browser")
}

# The host's "pdf" and "pages" when it has none of its own: printed here when the computer can, else "cannot do" (the
# builder then prints in the reader's browser, or shows the printable page)
.quire_default_pdf <- function(html, name) {
  if (!quire_can_print()) stop("The host cannot do PDFs here (no chromote, or no Chromium browser).", call. = FALSE)
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf), add = TRUE)
  quire_html_pdf(html, pdf)
  list(type = "application/pdf", data = jsonlite::base64_enc(readBin(pdf, "raw", file.info(pdf)$size)))
}

.quire_default_pages <- function(html, name) {
  if (!quire_can_print() || !requireNamespace("pdftools", quietly = TRUE)) {
    stop("The host cannot do pages here (no chromote or pdftools, or no Chromium browser).", call. = FALSE)
  }
  quire_print_pages(html)
}
