library(tibble)
library(scales)
library(lubridate)

test_that("Testing function to calculate enterovirus rate", {
  # initialize test variables
  sample_data <- dplyr::tibble(
    collect.date = c("2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01"),
    ADM0_NAME = c("country A", "country A", "country A", "country B", "country B", "country B"),
    site.name = c("site A", "site A", "site A", "site B", "site B", "site B"),
    ev.detect = c(0, 1, 0, 1, 1, 0),
    ADM1_NAME = c("prov 1", "prov 1", "prov 1", "prov 1", "prov 1", "prov 1"),
    ADM2_NAME = c("dist 1", "dist 1", "dist 1", "dist 1", "dist 1", "dist 1"),
    lat = c(1, 1, 1, 1, 1, 1),
    lng = c(1, 1, 1, 1, 1, 1)
  ) |>
    dplyr::mutate(collect.date = lubridate::as_date(collect.date))

  start_date <- "2022-01-01"
  end_date <- "2022-01-20"


  # check for required columns
  expect_error(
    f.ev.rate.01((sample_data |> select(!collect.date)),
      start.date = start_date,
      end.date = end_date
    ),
    info = "Expected error if missing collect.date"
  )
  expect_error(
    f.ev.rate.01((sample_data |> select(!ADM0_NAME)),
      start.date = start_date,
      end.date = end_date
    ),
    info = "Expected error if missing ADM0_NAME"
  )
  expect_error(
    f.ev.rate.01((sample_data |> select(!site.name)),
      start.date = start_date,
      end.date = end_date
    ),
    info = "Expected error if missing site.name"
  )
  expect_error(
    f.ev.rate.01((sample_data |> select(!ev.detect)),
      start.date = start_date,
      end.date = end_date
    ),
    info = "Expected error if missing ev.detect"
  )

  # check if calculations are correct and column types are as expected
  check_calculation <- tibble(
    ADM0_NAME = c("country A", "country B"),
    site.name = c("site A", "site B"),
    num.samples = c(3L, 3L),
    num.ev.pos = c(1L, 2L),
    ev.rate = c(1 / 3, 2 / 3),
    ev.percent = c("33%", "67%"),
    ev.rate.cat = c("<50%", "50 to <80%"),
    start.date = c("2022-01-01", "2022-01-01"),
    end.date = c("2022-01-20", "2022-01-20"),
    interval = c(19, 19),
    ADM1_NAME = c("prov 1", "prov 1"),
    ADM2_NAME = c("dist 1", "dist 1"),
    lat = c(1, 1),
    lng = c(1, 1)
  ) |> mutate(
    start.date = as_date(start.date),
    end.date = as_date(end.date),
    interval = end.date - start.date
  )

  expect_equal(
    f.ev.rate.01(sample_data,
      start.date = start_date,
      end.date = end_date
    ),
    check_calculation,
    info = "There is an error in the final output"
  )
})
