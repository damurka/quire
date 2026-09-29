#' A report theme from a PowerPoint or Word file
#'
#' Reads the colours and fonts of an Office theme or template (PowerPoint `.pptx` / `.potx`, Word `.docx` / `.dotx`)
#' into the fields of a report design, with the report builder's own reader (Quire's, run here through 'V8'), so a
#' theme read in R is the one the builder reads: the accent is the theme's first accent colour, the text colour its
#' first dark colour, the chart palette its six accent colours, the heading and body fonts its heading and body fonts.
#' A Word file's styles give the heading colour (Heading 1's), the fonts and the text sizes when they set them, and its
#' page size, orientation and margins. A PowerPoint file's master gives the title colour and fonts, the slide size and
#' the background colour, and its slides their designs: the logos, bands and title boxes they carry.
#'
#' @param path The file.
#' @param name The theme's name (by default the file's name without its extension).
#' @param default_accent The accent when the file's theme has none.
#' @return A list of the design fields the file sets: `template_kind` (`"pptx"` or `"docx"`), `theme`, `name`
#'   (`list(en, fr, pt)`), `accent`, `heading_color`, `text_color`, `muted_color`, `note_fill`, `note_border`,
#'   `apply_palette`, and when the file has them `heading_font`, `body_font`, `palette`. For a PowerPoint file also
#'   `background` (the master's background colour, or `NULL`), `slide_size` (`"16:9"` or `"4:3"`) and `slide_designs`
#'   (`list(title, content)`, each `NULL` or `list(background, decor, title, subtitle, body)`; a picture of `decor` has
#'   `src`, a data URL, and `name`). For a Word file the text sizes it sets (`body_size`, `h1_size`, `h2_size`,
#'   `title_size`, `caption_size`), `orientation`, `size` and `margins`.
#' @export
quire_theme_from_file <- function(path, name = NULL, default_accent = NULL) {
  if (!requireNamespace("V8", quietly = TRUE)) stop("quire_theme_from_file() needs the V8 package: install.packages(\"V8\").", call. = FALSE)
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop("This is not a PowerPoint or Word file (.pptx, .potx, .docx, .dotx).", call. = FALSE)
  }
  input <- list(
    data = jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size)),
    name = name %||% sub("\\.[^.]*$", "", basename(path)),
    defaultAccent = default_accent
  )
  ct <- .quire_context()
  ct$assign("__quireThemeInput", input, auto_unbox = TRUE, null = "null")
  out <- tryCatch(
    ct$eval("QuireExport.themeFromFile(__quireThemeInput).then(function (x) { return JSON.stringify(x); })", await = TRUE),
    error = function(e) stop(sub("^Error: ", "", conditionMessage(e)), call. = FALSE),
    finally = try(ct$eval("__quireThemeInput = null"), silent = TRUE)
  )
  .quire_doubles(jsonlite::fromJSON(out, simplifyVector = FALSE))
}

# Numbers as doubles (JSON's 20 is R's 20, not 20L): sizes and boxes are measures
.quire_doubles <- function(x) {
  if (is.list(x)) return(lapply(x, .quire_doubles))
  if (is.integer(x)) return(as.double(x))
  x
}
