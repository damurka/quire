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

test_that("the chart fields are filled from the chart they are about, on a page and on a slide", {
  host <- quire_host(
    kinds = function() list(quire_kind("coverage", "table", "Coverage", indicators = list(list(value = "anc4", label = "Antenatal care (4+)")))),
    render = function(request) quire_table(data.frame(Region = "North", Coverage = 81)),
    years = function() c(2023, 2024)
  )
  doc <- list(id = "r2", name = "R", lang = "en", design = list(), blocks = list(
    list(id = "p", type = "paragraph", text = "<p>{chart_indicator} in {chart_year}</p>"),
    list(id = "t", type = "table", kind = "coverage", indicator = "anc4")
  ))
  f <- tempfile(fileext = ".docx")
  quire_export(doc, host, f)
  expect_match(part(f, "word/document.xml"), "Antenatal care (4+) in 2024", fixed = TRUE)
  deck <- list(id = "d2", name = "D", kind = "deck", lang = "en", design = list(), blocks = list(), slides = list(list(id = "s1", items = list(
    list(id = "a", x = 1, y = 0.5, w = 8, h = 1, block = list(id = "a", type = "heading", text = "{chart_indicator}, {chart_year}")),
    list(id = "b", x = 1, y = 2, w = 8, h = 4, block = list(id = "b", type = "table", kind = "coverage", indicator = "anc4", year = 2023))))))
  p <- tempfile(fileext = ".pptx")
  quire_export(deck, host, p)
  expect_match(part(p, "ppt/slides/slide1.xml"), "Antenatal care (4+), 2023", fixed = TRUE)
})
