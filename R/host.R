#' Describe the app as the report builder's host
#'
#' The host answers the builder's few questions. Only `kinds` and `render` are needed; each other one adds a
#' feature (the text fields, report templates, keeping reports in the app, pictures, extra ribbon groups...).
#' Without `listReports`/`getReport`/`saveReport` the reports are kept in the reader's browser.
#'
#' @param kinds `function()`: the charts and tables the app can draw, a list of kinds, each a list with `kind`,
#'   `type` (`"chart"` or `"table"`), `label`, `group` and `groupLabel` (see [quire_kind()]).
#' @param render `function(request)`: draws one block. `request$block` is the block (its `kind`, and settings such
#'   as `year`, `region`, `options`), `request$width` and `request$height` its size in inches, `request$lang` the
#'   report's language. Returns [quire_plot()], [quire_table()] or [quire_error()].
#' @param fields `function(project, lang)`: the values of the text fields (`{country}`, `{report_date}`...), a named
#'   list.
#' @param fieldCatalog `function()`: the fields that can be inserted, a list of `list(key, label)`.
#' @param presets,preset `function(lang)`: the report templates; `function(id, lang, name)`: one as a new report.
#' @param themes `function()`: the report themes.
#' @param years,regions `function()`: the years and regions in the data (the charts' Year and Region lists).
#' @param flag `function()`: the country's flag, as a data URL, or `NULL`.
#' @param listReports,getReport,saveReport,deleteReport Keeping reports in the app: `function()` the saved
#'   reports (`list(id, name, updated, kind)` each), `function(id)` one report, `function(project)` save one,
#'   `function(id)` delete one. [quire_file_store()] makes them from a folder.
#' @param assetGet,assetSet Pictures kept once in the app: `function(id)`, `function(id, asset)`.
#' @param saveFile `function(file)`: a Word, PowerPoint or PDF file the builder made (`file$name`, `file$type`,
#'   `file$data` in base 64). Without it the reader's browser downloads the file.
#' @param extensions `function()`: the app's own ribbon groups, fields and actions.
#' @param action `function(id, context)`: one of the app's own actions was pressed.
#' @param ... Any other method of the host protocol, by name.
#' @return A host, to give [quire_server()].
#' @export
quire_host <- function(kinds, render, fields = NULL, fieldCatalog = NULL, presets = NULL, preset = NULL,
                       themes = NULL, years = NULL, regions = NULL, flag = NULL, listReports = NULL,
                       getReport = NULL, saveReport = NULL, deleteReport = NULL, assetGet = NULL,
                       assetSet = NULL, saveFile = NULL, extensions = NULL, action = NULL, ...) {
  fns <- c(
    list(kinds = kinds, render = render, fields = fields, fieldCatalog = fieldCatalog, presets = presets,
         preset = preset, themes = themes, years = years, regions = regions, flag = flag,
         listReports = listReports, getReport = getReport, saveReport = saveReport, deleteReport = deleteReport,
         assetGet = assetGet, assetSet = assetSet, saveFile = saveFile, extensions = extensions, action = action),
    list(...)
  )
  fns <- fns[!vapply(fns, is.null, logical(1))]
  bad <- names(fns)[!vapply(fns, is.function, logical(1))]
  if (length(bad)) stop(sprintf("quire_host(): %s must be a function.", paste(bad, collapse = ", ")), call. = FALSE)
  structure(fns, class = "quire_host")
}

#' A kind of chart or table the app draws
#'
#' @param kind Its id.
#' @param type `"chart"` or `"table"`.
#' @param label Its name, a string or a list of names by language (`list(en = "...", fr = "...")`).
#' @param group,groupLabel The group it is listed in (its id and name).
#' @param ... Anything else a kind may say: `year = TRUE` (it has a year), `regional = TRUE`, `indicators`,
#'   `levels`, `variants` (lists of `list(value, label)`), `defaults` (the block's settings when inserted).
#' @return A kind, a list.
#' @export
quire_kind <- function(kind, type = c("chart", "table"), label, group = "charts", groupLabel = group, ...) {
  type <- match.arg(type)
  c(list(kind = kind, type = type, label = label, group = group, groupLabel = groupLabel), list(...))
}
