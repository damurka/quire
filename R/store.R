#' Keep the reports and their pictures in a folder
#'
#' Makes the host's `listReports`, `getReport`, `saveReport`, `deleteReport`, `assetGet` and `assetSet`, keeping
#' each report as a JSON file in `dir` (its pictures in `dir/assets`), as the builder sends it. Give them to
#' [quire_host()] with `do.call()` or `...`.
#'
#' @param dir The folder (made if it is not there).
#' @return A named list of functions.
#' @export
quire_file_store <- function(dir) {
  dir.create(file.path(dir, "assets"), recursive = TRUE, showWarnings = FALSE)
  safe <- function(id) gsub("[^A-Za-z0-9_.-]", "_", id)
  path <- function(id) file.path(dir, paste0(safe(id), ".json"))
  read <- function(file) jsonlite::read_json(file, simplifyVector = FALSE)
  write <- function(x, file) jsonlite::write_json(x, file, auto_unbox = TRUE, null = "null", digits = NA, pretty = FALSE)
  list(
    listReports = function() {
      files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
      out <- lapply(files, function(f) {
        p <- tryCatch(read(f), error = function(e) NULL)
        if (is.null(p)) return(NULL)
        blocks <- p$blocks %||% list()
        # the charts and tables in it (a deck's are on its slides)
        items <- if (identical(p$kind, "deck")) unlist(lapply(p$slides %||% list(), function(sl) lapply(sl$items %||% list(), function(it) it$block)), recursive = FALSE) else blocks
        charts <- sum(vapply(items, function(b) isTRUE(b$type %in% c("chart", "table")), logical(1)))
        list(id = p$id, name = p$name %||% p$id, kind = p$kind %||% "document",
             updated = format(file.info(f)$mtime, "%Y-%m-%d %H:%M"),
             charts = charts, blocks = length(blocks))
      })
      out[!vapply(out, is.null, logical(1))]
    },
    getReport = function(id) if (file.exists(path(id))) read(path(id)) else NULL,
    saveReport = function(project) {
      write(project, path(project$id))
      invisible(NULL)
    },
    deleteReport = function(id) {
      unlink(path(id))
      invisible(NULL)
    },
    assetGet = function(id) {
      f <- file.path(dir, "assets", paste0(safe(id), ".json"))
      if (file.exists(f)) read(f) else NULL
    },
    assetSet = function(id, asset) {
      write(asset, file.path(dir, "assets", paste0(safe(id), ".json")))
      invisible(NULL)
    }
  )
}
