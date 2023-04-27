library(tidyverse)

# ES data
es.clean.01 <-  list.files(path = file.path("","","cdc.gov","project",
                                            "CGH_GID_Active","PEB","SIR","DATA",
                                            "Core 2.0","datafiles_01",
                                            "all_year_all_var_files_20220909"),
                           pattern="^(es).*(.rds)$", full.names=TRUE) %>%
  map_df(~read_rds(.x))

  es.clean.02 <- es.clean.01 %>% filter(ADM0_NAME == "NIGERIA")


# Country sequecing data

  library(readxl)
  ctry.seq.01 <- read_excel("//cdc.gov/project/CGH_GID_Active/PEB/SIR/DATA/Core 2.0/datafiles_01/special_projects/kppi_vis/Routine_lab_testing_locations.xlsx")
  ctry.seq.02 <- ctry.seq.01%>%  select(who.region, country, seq.capacity) %>%
    filter(!is.na(country)) %>%
    mutate(ctry = toupper(country)) %>%
    select(-country) %>%
    # There are a few countries where seq info is not available;
    # adding info for those countries.
    # Adding WPRO countries to the list
    mutate(
      seq.capacity = ifelse(ctry %in% c('AFGHANISTAN','IRAQ', 'MYANMAR', 'SOMALIA', 'SUDAN', 'SYRIAN ARAB REPUBLIC'),
                            'no', seq.capacity)
    )

  # Adding WPRO countries to the list
  wpro.ctry.seq.cat <- data.frame(c('WPRO', 'WPRO'),
                                  c('no', 'no'),
                                  c("PAPUA NEW GUINEA", "PHILIPPINES"))

  # Naming the above Data Frame
  names(wpro.ctry.seq.cat) <- c("who.region", "seq.capacity", "ctry")

  # Adding observations using rbind() function
  ctry.seq.03 <- rbind(ctry.seq.02, wpro.ctry.seq.cat)
  ctryseq.data <- ctry.seq.03[!duplicated(ctry.seq.03), ]

list(
  "es.data" = es.clean.02,
  "ctryseq.data" = ctryseq.data,
  "afp.data" = ctry.data$afp.all.2
) |>
  write_rds("//cdc.gov/project/CGH_GID_Active/PEB/SIR/DATA/Core 2.0/test_data/timely_detection_data.rds")

#start here
library(tidyverse)
library(lubridate)
library(here)
data <- read_rds("//cdc.gov/project/CGH_GID_Active/PEB/SIR/DATA/Core 2.0/test_data/timely_detection_data.rds")
source(here("R/f.timly.detection.01.R"))


afp.es.detect.ind <- f.timly.detection.01(
  afp.data = data$afp.data,
  es.data = data$es.data,
  ctryseq.data = data$ctryseq.data,
  start.date = "2019-06-01",
  end.date = "2022-01-31",
  rolling = T
)

overall.detection.01 <- afp.es.detect.ind[[1]]
overall.detection.02 <- afp.es.detect.ind[[2]]
