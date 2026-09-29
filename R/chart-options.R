.quire_chart_cache <- new.env(parent = emptyenv())

#' The chart options every Quire host shares
#'
#' The one description of the chart options (from Quire's contract, `chart-options.json`): each option's type and, for
#' a choice, its values; and the chart panel that edits them -- its elements, its settings with their labels in each
#' language, and its texts. The report builder has the same file, so a host that draws charts validates and lists the
#' options from here instead of keeping its own copy.
#'
#' @return A list: `options` (by name: `type`, and `choices` for a choice), `tabs` (the panel's elements: `key`,
#'   `label`, `icon`, and `show`, the option that shows or hides it), `fields` (its settings: `key`, `tab`, `group`,
#'   `label`, `type`, and `choices`, `keywords`, `step`, `min`, `max` when they have them) and `texts` (the panel's own
#'   words). Labels are lists by language (`en`, `fr`, `pt`).
#' @export
quire_chart_options <- function() {
  if (is.null(.quire_chart_cache$options)) {
    file <- system.file("contract", "chart-options.json", package = "quire")
    if (!nzchar(file)) stop("quire_chart_options(): the package has no chart-options.json (build it with npm run r-assets).", call. = FALSE)
    .quire_chart_cache$options <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  }
  .quire_chart_cache$options
}
