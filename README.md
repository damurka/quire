# quire (R)

Quire's report builder in a Shiny app. The app is the builder's host: it says what it can draw and draws it; the
builder does the editing, the page layout, the Word, PowerPoint and PDF files, and an AI's reading and editing.

```r
library(shiny)
library(quire)

host <- quire_host(
  kinds = function() list(quire_kind("by_region", "chart", "Coverage by region", year = TRUE)),
  render = function(request) {
    p <- ggplot2::ggplot(my_data(request$block$year), ggplot2::aes(region, value)) + ggplot2::geom_col()
    quire_plot(p, request)            # or quire_table(data_frame), or quire_error("why")
  }
)

ui <- fluidPage(quire_ui("reports"))
server <- function(input, output, session) quire_server("reports", host)
shinyApp(ui, server)
```

Only `kinds` and `render` are needed. The others add features: `fields` (the text fields), `presets`, `themes`,
`years`/`regions`, `extensions` and `action` (the app's own ribbon groups), and keeping reports in the app
(`listReports`, `getReport`, `saveReport`, `deleteReport`, `assetGet`, `assetSet`; `quire_file_store(dir)` makes them
from a folder). Without them the reports are kept in the reader's browser.

A full example: `shiny::runApp(system.file("examples", package = "quire"))`.

Licence: free to use with your apps, not to modify (see `LICENSE`); the libraries in the browser bundle are under their
own licences (`inst/www/THIRD-PARTY-NOTICES.txt`).
