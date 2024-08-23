test_that("testing f.stool.ad.02()", {
  # Creating a test dataset

  # All the required columns from AFP
  # "year", "adm0guid", "adm1guid", "adm2guid",
  # "stool1tostool2", "ontostool1", "ontostool2",
  # "stool.1.condition", "stool.2.condition",
  # "adequacy.01", "adequacy.02", "adequacy.03",
  # "stool1missing", "stool2missing",
  # "adequacy.final", "cdc.classification.all2"

  # Tests
  # 1. missing param
  # 2. bad.data param
  # 3. rolling param
  # 4. sp_continuity_validation param


  test_data_ctry <- dplyr::tibble(
    year = c(2021, 2021, 2021, 2022, 2022),
    adm0guid = c("ctry1", "ctry1", "ctry1", "ctry1", "ctry1"),
    stool1tostool2 = c(),
    ontostool1 = c(),
    ontostool2 = c(),
    stool.1.condition = c(),
    stool.2.condition = c(),
    adequacy.01 = c(),
    adequacy.02 = c(),
    adequacy.03 = c(),
    stool1missing = c(),
    stool2missing = c(),
    # made within the function
    # adequacy.final = c(),
    cdc.classification.all2 = c()
  )


  skip("To be written")
})
