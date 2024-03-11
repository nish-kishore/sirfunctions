test_that("testing f.metadata.tag() function", {
  library(tibble)
  library(ggplot2)
  library(flextable)

  # initialize test variables
  ggplot_fig <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
    geom_point()
  flextable_fig <- iris |>
    head(10) |>
    flextable()

  # error handling when there is no f tag available
  expect_error(f.metadata.tag(ggplot_fig),
    info = "raw.data and time_tag not provided. Expected an error."
  )

  # check that function works when raw.data is defined
  raw.data <- list(metadata = list(download_time = "2022-01-01"))
  expect_no_error(f.metadata.tag(ggplot_fig),
                  message = "raw.data was defined but generated an error")

  # check that function works when raw.data is not defined but a time_tag is
  expect_no_error(f.metadata.tag(ggplot_fig, time_tag = "2022-01-01 01:01:01 UTC"),
                  message = "time_tag provided but generated an error")
})
