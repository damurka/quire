# What the report builder asks its host, answered by the app's R functions. The builder sends a call
# (`list(kind = "call", id, method, args)`); the host's function for `method` is called with the call's arguments
# by name, and its value (or its error) goes back as the result. No Shiny here: [quire_server()] carries the messages.

# The host's functions and the arguments each is called with (by name), as the builder sends them.
.quire_methods <- list(
  kinds = character(), render = "request", fields = c("project", "lang"), fieldCatalog = character(),
  presets = "lang", preset = c("id", "lang", "name"), themes = character(), years = character(),
  regions = character(), flag = character(), dataMembers = character(), chartSchema = character(),
  themeFromFile = c("name", "data"), listReports = character(), getReport = "id", saveReport = "project",
  deleteReport = "id", assetGet = "id", assetSet = c("id", "asset"), saveFile = "file", pdf = c("html", "name"),
  extensions = character(), action = c("id", "context")
)

# The methods whose value is a JSON array: an R vector of one value would otherwise be sent as that value.
.quire_arrays <- c("kinds", "fieldCatalog", "presets", "themes", "years", "regions", "dataMembers", "listReports")

#' Answer one message from the report builder
#'
#' Calls the host's function for a call from the builder and makes its answer. [quire_server()] does this for
#' every message; it is exported for tests and for hosts that carry the messages themselves.
#'
#' @param host A host made with [quire_host()] (or a list of functions named as its arguments).
#' @param message A message from the builder, as a list: `kind`, `id`, `method`, `args`.
#' @return The result to send back (a list: `kind = "result"`, `id`, `ok`, and `result` or `error`), or `NULL`
#'   when the message is not a call.
#' @export
quire_dispatch <- function(host, message) {
  if (!is.list(message) || !identical(message$kind, "call")) return(NULL)
  id <- as.character(message$id)
  method <- as.character(message$method)
  fn <- host[[method]]
  if (!is.function(fn)) {
    return(list(kind = "result", id = id, ok = FALSE, error = sprintf('The host has no "%s".', method)))
  }
  args <- message$args
  if (!is.list(args)) args <- list()
  value <- tryCatch(
    .quire_call(fn, method, args),
    error = function(e) structure(list(message = conditionMessage(e)), class = "quire_failed")
  )
  if (inherits(value, "quire_failed")) {
    return(list(kind = "result", id = id, ok = FALSE, error = value$message))
  }
  if (method %in% .quire_arrays && is.atomic(value) && !is.null(value)) value <- I(value)
  list(kind = "result", id = id, ok = TRUE, result = value)
}

.quire_call <- function(fn, method, args) {
  wanted <- .quire_methods[[method]]
  if (is.null(wanted)) wanted <- names(args)
  given <- args[intersect(names(args), wanted)]
  formal <- names(formals(fn))
  if (!("..." %in% formal)) given <- given[intersect(names(given), formal)]
  do.call(fn, given)
}
