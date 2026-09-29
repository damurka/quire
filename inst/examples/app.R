# A Shiny app as Quire's host: two charts (ggplot2) and a table, reports kept in a folder.
#   shiny::runApp(system.file("examples", package = "quire"))

library(shiny)
library(quire)

coverage <- data.frame(
  region = rep(c("North", "Coast", "Lake", "Rift"), times = 5),
  year = rep(2020:2024, each = 4),
  value = c(61, 70, 75, 80, 63, 72, 78, 84, 65, 76, 82, 88, 67, 80, 86, 93, 76, 84, 91, 99)
)

host <- do.call(quire_host, c(
  list(
    kinds = function() list(
      quire_kind("by_region", "chart", list(en = "Coverage by region", fr = "Couverture par région"), "coverage", "Coverage", year = TRUE),
      quire_kind("over_time", "chart", list(en = "Coverage over time", fr = "Couverture dans le temps"), "coverage", "Coverage"),
      quire_kind("table", "table", list(en = "Coverage table", fr = "Tableau de couverture"), "coverage", "Coverage", year = TRUE)
    ),
    render = function(request) {
      b <- request$block
      year <- as.integer(b$year %||% 2024)
      if (identical(b$kind, "by_region")) {
        d <- coverage[coverage$year == year, ]
        p <- ggplot2::ggplot(d, ggplot2::aes(region, value)) +
          ggplot2::geom_col(fill = "#1f4e99") +
          ggplot2::geom_text(ggplot2::aes(label = paste0(value, "%")), vjust = -0.4, size = 3) +
          ggplot2::labs(x = NULL, y = "Coverage (%)") + ggplot2::theme_minimal()
        # the chart as data too: Word and PowerPoint have it as a real, editable chart
        chart <- list(type = "column", categories = I(d$region), series = list(list(name = "Coverage", values = I(d$value))), labels = TRUE)
        return(quire_plot(p, request, chart = chart))
      }
      if (identical(b$kind, "over_time")) {
        p <- ggplot2::ggplot(coverage, ggplot2::aes(year, value, colour = region)) + ggplot2::geom_line() +
          ggplot2::labs(x = NULL, y = "Coverage (%)", colour = NULL) + ggplot2::theme_minimal()
        return(quire_plot(p, request))
      }
      if (identical(b$kind, "table")) {
        d <- coverage[coverage$year == year, c("region", "value")]
        names(d) <- c("Region", "Coverage (%)")
        return(quire_table(d, footer = sprintf("Coverage in %d. Source: example data", year)))
      }
      quire_error(sprintf("No chart called %s", b$kind))
    },
    years = function() sort(unique(coverage$year)),
    regions = function() unique(coverage$region),
    fields = function(project, lang) list(country = "Examplia", report_date = format(Sys.Date(), "%B %Y"))
  ),
  quire_file_store(file.path(tempdir(), "quire-reports"))
))

`%||%` <- function(x, y) if (is.null(x)) y else x

ui <- fluidPage(quire_ui("reports"))
server <- function(input, output, session) quire_server("reports", host)

shinyApp(ui, server)
