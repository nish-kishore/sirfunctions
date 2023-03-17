library(tidyverse)
library(scales)
es.clean.01 <-  list.files(path = file.path("","","cdc.gov","project",
                                            "CGH_GID_Active","PEB","SIR","DATA",
                                            "Core 2.0","datafiles_01",
                                            "all_year_all_var_files_20220909"),
                           pattern="^(es).*(.rds)$", full.names=TRUE) %>%
  map_df(~read_rds(.x))

es.summary <- f.ev.rate.01(es.data = es.clean.01,
                           start.date = "2022-01-01",
                           end.date = "2022-12-31")


# Considerations for future dev for non-core data
## Should the ev.rate calc include guidance on how to handle NAs? We make the ev.detect
## var in pre-p and there is no NA option currently
## Should the variable names be included as parameters?
