skip_if_not_installed("V8")
skip_if_not_installed("magick")

export_host <- quire_host(
  kinds = function() list(quire_kind("coverage", "table", "Coverage")),
  render = function(request) {
    if (identical(request$block$kind, "broken")) stop("No data for this year")
    quire_table(data.frame(Region = c("North", "Coast"), Coverage = c(81, 90)), footer = "Source: example")
  },
  fields = function(project, lang) list(country = "Examplia")
)

report <- list(
  id = "r1", name = "Coverage review", lang = "en", design = list(),
  blocks = list(
    list(id = "h", type = "heading", level = 1, text = "Summary of {country}"),
    list(id = "t", type = "table", kind = "coverage"),
    list(id = "b", type = "table", kind = "broken")
  )
)

part <- function(file, name) {
  con <- unz(file, name)
  on.exit(close(con))
  paste(readLines(con, warn = FALSE), collapse = "")
}

test_that("a report is written as Word from R, the host's fields and tables in it", {
  f <- tempfile(fileext = ".docx")
  expect_equal(quire_export(report, export_host, f), f)
  body <- part(f, "word/document.xml")
  expect_match(body, "Summary of Examplia")
  expect_match(body, "North")
  expect_match(body, "Source: example")
  # a table the host cannot draw is marked, not fatal
  expect_match(body, "Could not be drawn")
})

test_that("the printable page and PowerPoint are written too", {
  h <- tempfile(fileext = ".html")
  quire_export(report, export_host, h, format = "html")
  expect_match(paste(readLines(h, warn = FALSE), collapse = ""), "Summary of Examplia")
  deck <- list(id = "d1", name = "Deck", kind = "deck", lang = "en", design = list(), blocks = list(),
               slides = list(list(id = "s1", items = list(list(id = "t1", x = 1, y = 1, w = 8, h = 1,
                                                               block = list(id = "t1", type = "heading", text = "{country}"))))))
  p <- tempfile(fileext = ".pptx")
  quire_export(deck, export_host, p, format = "docx")
  expect_true("ppt/slides/slide1.xml" %in% utils::unzip(p, list = TRUE)$Name)
  expect_match(part(p, "ppt/slides/slide1.xml"), "Examplia")
})

test_that("a PDF needs a converter", {
  expect_error(quire_export(report, export_host, tempfile(fileext = ".pdf"), format = "pdf"), "converts it")
})
