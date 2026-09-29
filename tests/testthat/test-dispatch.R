withr_tempdir <- function() {
  d <- tempfile("quire-test-")
  dir.create(d)
  d
}

call <- function(method, args = list(), id = "1") list(kind = "call", id = id, method = method, args = args)

host <- quire_host(
  kinds = function() list(quire_kind("coverage", "chart", "Coverage")),
  render = function(request) {
    if (identical(request$block$kind, "broken")) stop("No data for this year")
    quire_table(data.frame(Region = c("North", "Coast"), Coverage = c(81, 90)), footer = "Source: example")
  },
  years = function() 2024,
  fields = function(project, lang) list(country = if (identical(lang, "fr")) "Exemplie" else "Examplia"),
  saveReport = function(project) NULL
)

test_that("a call is answered by the host's function, called with its arguments by name", {
  r <- quire_dispatch(host, call("fields", list(project = list(id = "r1"), lang = "fr"), id = "7"))
  expect_equal(r$kind, "result")
  expect_equal(r$id, "7")
  expect_true(r$ok)
  expect_equal(r$result$country, "Exemplie")
})

test_that("an array stays an array even with one value", {
  r <- quire_dispatch(host, call("years"))
  expect_true(inherits(r$result, "AsIs"))
  expect_equal(as.character(jsonlite::toJSON(r$result, auto_unbox = TRUE)), "[2024]")
})

test_that("an error or a missing method is a failed result with its reason, not a crash", {
  r <- quire_dispatch(host, call("render", list(request = list(block = list(kind = "broken")))))
  expect_false(r$ok)
  expect_match(r$error, "No data for this year")
  r <- quire_dispatch(host, call("themes"))
  expect_false(r$ok)
  expect_match(r$error, "has no \"themes\"")
  expect_null(quire_dispatch(host, list(kind = "event", name = "hello")))
})

test_that("a table is its header, its rows (numbers kept as values) and its footer", {
  t <- quire_dispatch(host, call("render", list(request = list(block = list(kind = "coverage")))))$result
  expect_equal(t$kind, "table")
  expect_equal(t$header[[1]][[2]]$text, "Coverage")
  expect_equal(t$rows[[2]][[2]]$value, 90)
  expect_equal(t$rows[[2]][[2]]$text, "90")
  expect_equal(t$footer, "Source: example")
})

test_that("a plot is drawn at the size asked for", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(data.frame(x = c("a", "b"), y = c(1, 2)), ggplot2::aes(x, y)) + ggplot2::geom_col()
  d <- quire_plot(p, list(width = 5, height = 3))
  expect_equal(d$kind, "image")
  expect_equal(c(d$w, d$h), c(5, 3))
  expect_true(!is.null(d$svg) || grepl("^data:image/png;base64,", d$src))
  if (!is.null(d$svg)) expect_match(d$svg, "<svg")
})

test_that("a folder keeps the reports as the builder sends them (arrays of one stay arrays)", {
  dir <- withr_tempdir()
  store <- quire_file_store(dir)
  project <- list(id = "r 1", name = "Review", kind = "document", blocks = list(list(id = "h", type = "heading", text = "Hi")), design = list(palette = list("#7d3f40")))
  store$saveReport(project)
  back <- store$getReport("r 1")
  expect_equal(back$name, "Review")
  expect_length(back$blocks, 1)
  expect_true(is.list(back$design$palette))
  expect_equal(store$listReports()[[1]]$blocks, 1)
  expect_equal(store$listReports()[[1]]$charts, 0)
  store$assetSet("a1", list(type = "image/png", data = "AAA"))
  expect_equal(store$assetGet("a1")$data, "AAA")
  store$deleteReport("r 1")
  expect_length(store$listReports(), 0)
})

test_that("a host's functions must be functions", {
  expect_error(quire_host(kinds = list(), render = function(request) NULL), "kinds must be a function")
})

test_that("the UI mounts the builder with Shiny's messages in the module's namespace", {
  ui <- as.character(quire_ui("reports", lang = "fr"))
  expect_match(ui, 'id="reports-studio"')
  expect_match(ui, 'Quire.shiny\\("reports-"\\)')
  expect_match(ui, '"lang":"fr"')
})
