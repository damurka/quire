#' Quire's scripts and styles
#'
#' The report builder's browser bundle (`quire.js`), its styles and fonts, as an HTML dependency. [quire_ui()]
#' includes it; call it yourself only to load the builder in a page of your own.
#'
#' @return An [htmltools::htmlDependency()].
#' @export
quire_dependency <- function() {
  htmltools::htmlDependency(
    name = "quire",
    version = as.character(utils::packageVersion("quire")),
    src = c(file = system.file("www", package = "quire")),
    script = "quire.js",
    stylesheet = "quire.css"
  )
}

#' The report builder in a Shiny app's UI
#'
#' @param id The module's id (as given to [quire_server()]).
#' @param lang The interface's language (`"en"`, `"fr"`, `"pt"`).
#' @param theme The interface's look: a list of design tokens (`primary`, `fontSans`, `radius`, `mode = "dark"`...).
#'   The report's own design is the report's.
#' @param ... Anything else the builder takes (`open`: a report to open; `aiEnabled`...).
#' @return The UI, to put in the app's page.
#' @export
quire_ui <- function(id, lang = "en", theme = NULL, ...) {
  ns <- shiny::NS(id)
  opts <- c(list(lang = lang), if (!is.null(theme)) list(theme = theme), list(...))
  js <- sprintf(
    "(function(){var el=document.getElementById(%s);var o=%s;o.transport=Quire.shiny(%s);Quire.mount(el,'ReportStudio',o);})();",
    jsonlite::toJSON(ns("studio"), auto_unbox = TRUE),
    jsonlite::toJSON(opts, auto_unbox = TRUE, null = "null"),
    jsonlite::toJSON(ns(""), auto_unbox = TRUE)
  )
  htmltools::tagList(
    quire_dependency(),
    htmltools::div(id = ns("studio"), class = "quire-studio"),
    htmltools::tags$script(htmltools::HTML(js))
  )
}

#' The report builder's host in a Shiny app's server
#'
#' Answers the builder's calls with the host's functions, in the module `id`.
#'
#' @param id The module's id (as given to [quire_ui()]).
#' @param host The host, made with [quire_host()].
#' @param on_event Optionally, `function(name, data)`, told what happens in the builder: `"ai.ask"` (the reader asked
#'   the AI: `data$prompt`, `data$about`), `"report.opened"` (`data$project`, an id or `NULL`), `"lang.changed"`.
#' @return A list of functions to tell the builder things: `send(name, data)` sends an event (`"data.changed"`
#'   when the app's data changed, so the charts are drawn again; `"report.changed"` with `list(project = ...)` when a
#'   report changed elsewhere; `"reports.changed"` when the list of reports did; `"report.open"` with `list(id = ...)`
#'   to open one; `"lang"` with `list(lang = ...)`), `dataChanged()` as a shortcut.
#' @export
quire_server <- function(id, host, on_event = NULL) {
  if (!inherits(host, "quire_host")) host <- do.call(quire_host, host)
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$quire, {
      m <- input$quire
      if (identical(m$kind, "event")) {
        if (is.function(on_event)) on_event(as.character(m$name), m$data %||% list())
        return()
      }
      reply <- quire_dispatch(host, m)
      if (!is.null(reply)) session$sendCustomMessage("quire", reply)
      # (not ignoreInit: a message that comes before the observer first runs is still answered; the input starts NULL)
    })
    send <- function(name, data = list()) {
      session$sendCustomMessage("quire", list(kind = "event", name = name, data = if (length(data)) data else structure(list(), names = character())))
    }
    list(send = send, dataChanged = function() send("data.changed"))
  })
}
