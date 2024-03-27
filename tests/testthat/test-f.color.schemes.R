test_that("testing f.color.schemes() work", {
  # test for invalid color scheme
  expect_error(f.color.schemes("invalid_scheme"),
    info = "Message: expected an error."
  )

  epicurve <- c(
    "WILD 1" = "#e41a1c",
    "cVDPV 2" = "#4daf4a",
    "VDPV 2" = "#a6d854",
    "cVDPV 1" = "#377eb8",
    "VDPV 1" = "#80b1d3",
    "COMPATIBLE" = "#ff7f00",
    "PENDING" = "#993404",
    "LAB PENDING" = "#fccde5",
    "NPAFP" = "#666666",
    "NOT-AFP" = "#beaed4"
  )
  expect_equal(f.color.schemes("epicurve"), epicurve, info = "did not match color scheme")

  para.case <- c(
    "WILD 1" = "#e41a1c",
    "cVDPV 2" = "#4daf4a",
    "VDPV 2" = "#a6d854",
    "cVDPV 1" = "#377eb8",
    "VDPV 1" = "#80b1d3",
    "COMPATIBLE" = "#ff7f00"
  )
  expect_equal(f.color.schemes("para.case"), para.case, info = "did not match color scheme")

  afp.prov <- c(
    "0" = "grey",
    "1" = "#ffffcc",
    "2-5" = "#a1dab4",
    "6-9" = "#41b6c4",
    "10+" = "#225ea8"
  )
  expect_equal(f.color.schemes("afp.prov"), afp.prov, info = "did not match color scheme")

  afp.dist <- c(
    # "0"="grey",
    "1" = "#ffffcc",
    "2" = "#a1dab4",
    "3-5" = "#41b6c4",
    "6+" = "#225ea8"
  )
  expect_equal(f.color.schemes("afp.dist"), afp.dist, info = "did not match color scheme")

  pot.comp <- c(
    "COMPATIBLE" = "#ff7f00",
    "POTENTIALLY COMPATIBLE" = "#984ea3"
  )
  expect_equal(f.color.schemes("pot.comp"), pot.comp, info = "did not match color scheme")

  silence <- c(
    "4" = "#d7191c",
    "3" = "#fdae61",
    "2" = "#ffffbf",
    "1" = "#abd9e9",
    "0" = "#2c7bb6"
  )
  expect_equal(f.color.schemes("silence"), silence, info = "did not match color scheme")

  silence.v2 <- c(
    "4" = "#54278f",
    "3" = "#756bb1",
    "2" = "#9e9ac8",
    "1" = "#cbc9e2",
    "0" = "#f2f0f7"
  )
  expect_equal(f.color.schemes("silence.v2"), silence.v2, info = "did not match color scheme")

  cases <- c(
    "COMPATIBLE" = "black",
    "WILD 1" = "red",
    "cVDPV 2" = "dark green",
    "VDPV 2" = "light green",
    "VDPV 1" = "light blue",
    "cVDPV 1" = "dark blue"
  )
  expect_equal(f.color.schemes("cases"), cases, info = "did not match color scheme")

  es <- c(
    "<50%" = "red",
    "50-79%" = "#feb24c",
    "80-100%" = "#0070c0",
    "<5 samples" = "black"
  )
  expect_equal(f.color.schemes("es"), es, info = "did not match color scheme")
})
