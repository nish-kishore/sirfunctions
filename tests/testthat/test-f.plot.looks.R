test_that("testing f.plot.looks", {

  plot_class <- ggplot2::theme()
  unrecognized_option_message <- "Expected a vector of class theme and gg"

  # Check for supported plot types
  expect_no_error(f.plot.looks("02"), message = "Valid type but error raised.")
  expect_no_error(f.plot.looks("epicurve"), message = "Valid type but error raised.")
  expect_no_error(f.plot.looks("geomtile"), message = "Valid type but error raised.")
  expect_no_error(f.plot.looks("gpln_type1"), message = "Valid type but error raised.")
  expect_no_error(f.plot.looks("gpln_type2"), message = "Valid type but error raised.")


  # Check if error gets caught as appropriate
  expect_error(f.plot.looks("invalid"),
               info = " 'invalid' is not a supported plot type.")



})
