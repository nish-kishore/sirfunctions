test_that("testing f.npafp.rate.01()", {
  # Initialize testing values
  country_name <- "SIERRA LEONE"
  raw_data <- sirfunctions::get_all_polio_data()
  country_data <- sirfunctions::extract_country_data(
    .country = country_name,
    .raw.data = raw_data
  )
  afp_data <- country_data$afp.all.2
  c_afp_data <- afp_data |>
    select(
      adm0guid, adm1guid, adm2guid, date, cdc.classification.all2, epid,
      ctry, prov, dist
    )
  cpop_data <- raw_data$ctry.pop |>
    select(ADM0_NAME, adm0guid, u15pop, year, datasource) |>
    rename("ctry" = "ADM0_NAME") |>
    filter(
      ctry == str_trim(str_to_upper(country_name)),
      between(year, year(start_date), year(end_date))
    )

  ppop_data <- raw_data$prov.pop |>
    select(
      ADM0_NAME, ADM0_GUID, ADM1_NAME, adm1guid, u15pop.prov, year,
      datasource
    ) |>
    rename(
      "adm0guid" = "ADM0_GUID",
      "ctry" = "ADM0_NAME",
      "prov" = "ADM1_NAME",
      "u15pop" = "u15pop.prov"
    ) |>
    filter(
      ctry == str_trim(str_to_upper(country)),
      between(year, year(start_date), year(end_date))
    )

  dpop_data <- raw_data$dist.pop |>
    select(
      ADM0_NAME, ADM0_GUID, ADM1_NAME, adm1guid, ADM2_NAME, adm2guid,
      u15pop, year, datasource
    ) |>
    rename(
      "adm0guid" = "ADM0_GUID",
      "ctry" = "ADM0_NAME",
      "prov" = "ADM1_NAME",
      "dist" = "ADM2_NAME"
    ) |>
    filter(
      ctry == str_trim(str_to_upper(country)),
      between(year, year(start_date), year(end_date))
    )


  start_date <- "2022-01-01"
  end_date <- "2022-12-31"

  # Check if datasets are attached
  expect_error(f.npafp.rate.01(start.date = start_date, end.date = end_date, spatial.scale = "ctry"),
    info = "Both datasets are not attached and should have thrown an error."
  )
  expect_error(f.npafp.rate.01(afp.data = afp_data, start.date = start_date, end.date = end_date, spatial.scale = "ctry"),
    info = "pop data not attached and should have thrown an error."
  )
  expect_error(f.npafp.rate.01(pop.data = pop_data, start.date = start_date, end.date = end_date, spatial.scale = "ctry"),
    info = "afp data not attached and should have thrown an error."
  )

  # Check that the date formats are correct

  expect_no_error(
    f.npafp.rate.01(
      afp.data = afp_data,
      pop.data = cpop_data,
      start.date = start_date,
      end.date = end_date,
      spatial.scale = "ctry"
    ),
    info = "threw an error despite valid arguments."
  )
  start_date <- "01-1000-01"
  end_date <- "2022-12-31"
  expect_error(
    f.npafp.rate.01(
      afp.data = afp_data,
      pop.data = cpop_data,
      start.date = start_date,
      end.date = end_date,
      spatial.scale = "ctry"
    ),
    info = "start.date is incorrect format, should throw an error"
  )

  start_date <- "2022-01-01"
  end_date <- "o1-02-cder"
  expect_error(
    f.npafp.rate.01(
      afp.data = afp_data,
      pop.data = cpop_data,
      start.date = start_date,
      end.date = end_date,
      spatial.scale = "ctry"
    ),
    info = "end.date is incorrect format, should throw an error"
  )

  # reset to valid dates
  end_date <- "2022-12-31"

  # Check that population data set contain all the years of analysis
  expect_no_error(
    f.npafp.rate.01(
      afp.data = afp_data,
      pop.data = cpop_data,
      start.date = start_date,
      end.date = end_date,
      spatial.scale = "ctry"
    ),
    message = "Threw an error despite valid arguments"
  )
  expect_error(
    f.npafp.rate.01(
      afp.data = afp_data,
      pop.data = cpop_data,
      start.date = start_date,
      end.date = end_date,
      spatial.scale = "ctry"
    ),
    info = "pop.data does not contain all year in AFP data. Should throw an error."
  )

  # Check for valid argument for spatial.data
  expect_no_error(
    f.npafp.rate.01(
      afp.data = afp_data,
      pop.data = cpop_data,
      start.date = start_date,
      end.date = end_date,
      spatial.scale = "ctry"
    ),
    message = "'ctry' is a valid argument but error is thrown"
  )
  expect_no_error(
    f.npafp.rate.01(
      afp.data = afp_data,
      pop.data = ppop_data,
      start.date = start_date,
      end.date = end_date,
      spatial.scale = "prov"
    ),
    message = "'prov' is a valid argument but error is thrown"
  )
  expect_no_error(
    f.npafp.rate.01(
      afp.data = afp_data,
      pop.data = dpop_data,
      start.date = start_date,
      end.date = end_date,
      spatial.scale = "dist"
    ),
    message = "'dist' is a valid argument but error is thrown"
  )

  expect_error(
    f.npafp.rate.01(
      afp.data = afp_data,
      pop.data = cpop_data,
      start.date = start_date,
      end.date = end_date,
      spatial.scale = "invalid"
    ),
    message = "'invalid' is an invalid argument but  no error is thrown"
  )
})
