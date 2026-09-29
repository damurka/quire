test_that("the chart options are the contract's", {
  o <- quire_chart_options()
  expect_true(all(c("options", "tabs", "fields", "texts") %in% names(o)))
  expect_equal(o$options$title_face$type, "choice")
  expect_true("bold" %in% unlist(o$options$title_face$choices))
  keys <- vapply(o$fields, function(f) f$key, character(1))
  expect_true(all(setdiff(keys, "entries") %in% names(o$options)))
  expect_type(o$fields[[1]]$label$fr, "character")
})

test_that("a file that is not a PowerPoint or Word file is refused", {
  expect_error(quire_theme_from_file(tempfile()), "PowerPoint or Word")
  skip_if_not_installed("V8")
  f <- tempfile(fileext = ".pptx")
  writeLines("not a zip", f)
  expect_error(quire_theme_from_file(f), "PowerPoint or Word")
})
