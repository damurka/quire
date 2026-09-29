#' A chart drawn for the report builder
#'
#' Draws a plot at the size the builder asks for. With 'svglite' installed the drawing is an SVG (sharp at any zoom,
#' and made a picture in the files by the builder); without it, a PNG.
#'
#' @param plot A 'ggplot2' plot, or a function that draws on the current device.
#' @param request The render request (its `width` and `height`, in inches).
#' @param chart Optionally, the chart as data (`list(type, categories, series, ...)`): Word and PowerPoint then
#'   have it as a real chart, its data editable there.
#' @param dpi The PNG's dots per inch (without 'svglite').
#' @return A drawing, to return from the host's `render`.
#' @export
quire_plot <- function(plot, request, chart = NULL, dpi = 144) {
  w <- as.numeric(request$width %||% 6.5)
  h <- as.numeric(request$height %||% 3.7)
  draw <- function() if (is.function(plot)) plot() else print(plot)
  out <- if (requireNamespace("svglite", quietly = TRUE)) {
    svg <- svglite::svgstring(width = w, height = h, standalone = TRUE)
    tryCatch(draw(), finally = grDevices::dev.off())
    list(kind = "image", svg = as.character(svg()), w = w, h = h)
  } else {
    file <- tempfile(fileext = ".png")
    on.exit(unlink(file), add = TRUE)
    grDevices::png(file, width = w * dpi, height = h * dpi, res = dpi)
    tryCatch(draw(), finally = grDevices::dev.off())
    bytes <- readBin(file, "raw", file.info(file)$size)
    list(kind = "image", src = paste0("data:image/png;base64,", jsonlite::base64_enc(bytes)), w = w, h = h)
  }
  if (!is.null(chart)) out$chart <- chart
  out
}

#' A table drawn for the report builder
#'
#' The builder draws the table itself, in the report's style and its Table Design; the host gives its cells.
#'
#' @param data A data frame: its column names are the header, its rows the table's rows (numbers kept as values,
#'   shown as `format()` gives them unless `text` says otherwise).
#' @param footer A note under the table (its source), or `NULL`.
#' @param widths The columns' shares of the width (summing to 1), or `NULL` for equal columns.
#' @param text Optionally, a data frame of the same shape with the cells' texts.
#' @return A table, to return from the host's `render`.
#' @export
quire_table <- function(data, footer = NULL, widths = NULL, text = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  shown <- if (is.null(text)) as.data.frame(lapply(data, function(x) if (is.numeric(x)) format(x, trim = TRUE) else as.character(x)), stringsAsFactors = FALSE) else as.data.frame(text, stringsAsFactors = FALSE)
  header <- list(lapply(names(data), function(n) list(text = n)))
  rows <- lapply(seq_len(nrow(data)), function(i) {
    lapply(seq_along(data), function(j) {
      cell <- list(text = as.character(shown[i, j]))
      v <- data[[j]][i]
      if (is.numeric(v) && !is.na(v)) cell$value <- v
      cell
    })
  })
  out <- list(kind = "table", header = header, rows = rows)
  if (!is.null(footer)) out$footer <- footer
  if (!is.null(widths)) out$widths <- I(as.numeric(widths))
  out
}

#' A block the host could not draw
#'
#' @param message Why, for the reader.
#' @return An error drawing, shown in the block's place.
#' @export
quire_error <- function(message) list(kind = "error", message = as.character(message))

`%||%` <- function(x, y) if (is.null(x)) y else x
