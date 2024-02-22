test_that("get_all_polio_data parameters work as expected", {
  # testing that size parameter works without attaching spatial data
  expect_no_error(get_all_polio_data(size = "small",
                                     folder = "GID/PEB/SIR/Data/",
                                     force.new.run = F,
                                     recreate.static.files = F,
                                     attach.spatial.data = F)
                  )
  expect_no_error(get_all_polio_data(size = "medium",
                                     folder = "GID/PEB/SIR/Data/",
                                     force.new.run = F,
                                     recreate.static.files = F,
                                     attach.spatial.data = F)
  )
  expect_no_error(get_all_polio_data(size = "large",
                                     folder = "GID/PEB/SIR/Data/",
                                     force.new.run = F,
                                     recreate.static.files = F,
                                     attach.spatial.data = F)
  )
  expect_error(get_all_polio_data(   size = "large  ",
                                     folder = "GID/PEB/SIR/Data/",
                                     force.new.run = F,
                                     recreate.static.files = F,
                                     attach.spatial.data = F)
  )

  # test that size parameter works attaching spatial data
  expect_no_error(get_all_polio_data(size = "small",
                                     folder = "GID/PEB/SIR/Data/",
                                     force.new.run = F,
                                     recreate.static.files = F,
                                     attach.spatial.data = T)
  )
  expect_no_error(get_all_polio_data(size = "medium",
                                     folder = "GID/PEB/SIR/Data/",
                                     force.new.run = F,
                                     recreate.static.files = F,
                                     attach.spatial.data = T)
  )
  expect_no_error(get_all_polio_data(size = "large",
                                     folder = "GID/PEB/SIR/Data/",
                                     force.new.run = F,
                                     recreate.static.files = F,
                                     attach.spatial.data = T)
  )
})
