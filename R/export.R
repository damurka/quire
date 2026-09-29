#' Write a report as a Word, PowerPoint, PDF or printable file, from R
#'
#' The same writers as the builder's downloads (Quire's), run in R through 'V8', with no browser: a report written
#' here is the file the builder would have made. The host draws the report's charts and tables as it does for the
#' builder (its `render`), gives its fields and pictures; the pictures are turned and cropped with 'magick', SVG
#' drawings made pictures with 'rsvg'.
#'
#' @param project The report (a list, as saved), or its id (the host's `getReport` finds it).
#' @param host The host, made with [quire_host()].
#' @param file Where to write it.
#' @param format `"docx"` (Word; a slide deck is written as PowerPoint), `"pptx"`, `"pdf"` or `"html"` (the
#'   printable page).
#' @param lang The language of the report's texts when it has none of its own.
#' @param fields The fields' values; by default the host's `fields`.
#' @param pdf For a PDF: `function(input, output)` that converts the Word (or PowerPoint) file `input` to the PDF
#'   `output` (Word or LibreOffice).
#' @return `file`, invisibly.
#' @export
quire_export <- function(project, host, file, format = c("docx", "pptx", "pdf", "html"), lang = NULL, fields = NULL,
                         pdf = NULL) {
  format <- match.arg(format)
  for (p in c("V8", "magick")) {
    if (!requireNamespace(p, quietly = TRUE)) stop(sprintf("quire_export() needs the %s package: install.packages(\"%s\").", p, p), call. = FALSE)
  }
  if (!inherits(host, "quire_host")) host <- do.call(quire_host, host)
  if (is.character(project)) {
    if (!is.function(host$getReport)) stop("A report given by its id needs the host's getReport.", call. = FALSE)
    id <- project
    project <- host$getReport(id)
    if (is.null(project)) stop(sprintf("There is no report %s.", id), call. = FALSE)
  }
  deck <- identical(project$kind, "deck")
  as <- if (format == "pdf" || (format == "docx" && deck)) (if (deck) "pptx" else "docx") else format
  if (format == "pdf" && !is.function(pdf)) {
    stop("A PDF is made from the Word or PowerPoint file: give `pdf`, a function(input, output) that converts it (Word or LibreOffice).", call. = FALSE)
  }
  old <- .quire_v8$host
  .quire_v8$host <- host
  on.exit(.quire_v8$host <- old, add = TRUE)
  input <- list(project = project, format = as, lang = lang, fields = fields)
  json <- jsonlite::toJSON(input, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  out <- .quire_context()$eval(sprintf("QuireExport.write(%s).then(function (x) { return JSON.stringify(x); })", json), await = TRUE)
  res <- jsonlite::fromJSON(out)
  target <- if (format == "pdf") tempfile(fileext = paste0(".", as)) else file
  writeBin(jsonlite::base64_dec(res$data), target)
  if (format == "pdf") {
    on.exit(unlink(target), add = TRUE)
    pdf(target, file)
    if (!file.exists(file)) stop("The PDF was not made.", call. = FALSE)
  }
  invisible(file)
}

# ---- what the writers ask R, through V8 ----------------------------------------------------------------------------

.quire_v8 <- new.env(parent = emptyenv())

# One V8 context with the writers loaded, kept for the session
.quire_context <- function() {
  if (is.null(.quire_v8$ct)) {
    ct <- V8::v8()
    ct$source(system.file("js", "quire-export.js", package = "quire"))
    .quire_v8$ct <- ct
  }
  .quire_v8$ct
}

.json_out <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA)

# the host's answer to one question (render, fields, flag, assetGet)
.v8_call <- function(json) {
  p <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  reply <- quire_dispatch(.quire_v8$host, list(kind = "call", id = "v8", method = p$method, args = p$args %||% list()))
  .json_out(list(ok = isTRUE(reply$ok), result = reply$result, error = reply$error))
}

# a picture's bytes from its data URL
.v8_image <- function(src) {
  if (!is.character(src) || !startsWith(src, "data:")) stop("A picture is given as a data URL.")
  bytes <- if (grepl("^data:[^,]*;base64,", src)) jsonlite::base64_dec(sub("^data:[^,]*,", "", src)) else charToRaw(utils::URLdecode(sub("^data:[^,]*,", "", src)))
  magick::image_read(bytes)
}

.v8_svg_png <- function(json) {
  p <- jsonlite::fromJSON(json)
  w <- max(1, round(p$w * p$dpi))
  h <- max(1, round(p$h * p$dpi))
  png <- if (requireNamespace("rsvg", quietly = TRUE)) {
    rsvg::rsvg_png(charToRaw(enc2utf8(p$svg)), width = w, height = h)
  } else {
    img <- magick::image_read_svg(p$svg, width = w, height = h)
    magick::image_write(magick::image_background(img, "white"), format = "png")
  }
  .json_out(list(data = jsonlite::base64_enc(png)))
}

.v8_size <- function(json) {
  p <- jsonlite::fromJSON(json)
  info <- magick::image_info(.v8_image(p$src))
  .json_out(list(w = info$width[[1]], h = info$height[[1]]))
}

# a picture turned (quarter turns), flipped, cropped (fractions off the top, right, bottom and left; a circle: its
# middle square), at about `dpi` for its size in the file, as the builder's canvas does it
.v8_transform <- function(json) {
  p <- jsonlite::fromJSON(json)
  img <- .v8_image(p$src)
  if (isTRUE(p$rotate %% 360 != 0)) img <- magick::image_rotate(img, p$rotate %% 360)
  if (isTRUE(p$flipV)) img <- magick::image_flip(img)
  if (isTRUE(p$flipH)) img <- magick::image_flop(img)
  info <- magick::image_info(img)
  fw <- info$width[[1]]
  fh <- info$height[[1]]
  crop <- as.numeric(unlist(p$crop %||% c(0, 0, 0, 0)))
  cw <- max(1, round(fw * (1 - crop[2] - crop[4])))
  ch <- max(1, round(fh * (1 - crop[1] - crop[3])))
  x <- round(fw * crop[4])
  y <- round(fh * crop[1])
  if (isTRUE(p$circle)) {
    side <- min(cw, ch)
    x <- x + floor((cw - side) / 2)
    y <- y + floor((ch - side) / 2)
    cw <- ch <- side
  }
  img <- magick::image_crop(img, magick::geometry_area(cw, ch, x, y))
  k <- min(4, (max(p$w, p$h) * p$dpi) / max(cw, ch), 2400 / max(cw, ch))
  img <- magick::image_scale(img, sprintf("%dx%d!", max(1, round(cw * k)), max(1, round(ch * k))))
  if (isTRUE(p$circle)) {
    s <- magick::image_info(img)$width[[1]]
    mask <- magick::image_draw(magick::image_blank(s, s, "black"))
    graphics::symbols(s / 2, s / 2, circles = s / 2, inches = FALSE, add = TRUE, bg = "white", fg = NA)
    grDevices::dev.off()
    img <- magick::image_composite(magick::image_transparent(img, "none"), mask, operator = "CopyOpacity")
  }
  jpeg <- grepl("^data:image/jpe?g", p$src) && !isTRUE(p$circle)
  bytes <- magick::image_write(img, format = if (jpeg) "jpeg" else "png", quality = if (jpeg) 92 else NULL)
  .json_out(list(data = jsonlite::base64_enc(bytes), type = if (jpeg) "jpg" else "png"))
}
