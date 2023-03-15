pop.data.dist <- read_rds("//cdc.gov/project/CGH_GID_Active/PEB/SIR/DATA/Core 2.0/datafiles_01/dist.pop.long2010_2022.rds") |>
  select(year, ctry = ADM0_NAME, prov = ADM1_NAME, dist = ADM2_NAME, 
         adm0guid = ADM0_GUID, adm1guid, adm2guid, u15pop) |> 
  filter(ctry == "NIGERIA")

pop.data.prov <- read_rds("//cdc.gov/project/CGH_GID_Active/PEB/SIR/DATA/Core 2.0/datafiles_01/prov.pop.long2010_2022.rds") |>
  select(year, ctry = ADM0_NAME, prov = ADM1_NAME, 
         adm0guid = ADM0_GUID, adm1guid, u15pop = u15pop.prov) |> 
  filter(ctry == "NIGERIA")

pop.data.ctry <- read_rds("//cdc.gov/project/CGH_GID_Active/PEB/SIR/DATA/Core 2.0/datafiles_01/ctry.pop.2000_2022.rds") |>
  select(year, ctry = ADM0_NAME, adm0guid, u15pop) |> 
  filter(ctry == "NIGERIA")

list(
  "pop.data.dist" = pop.data.dist, 
  "pop.data.prov" = pop.data.prov, 
  "pop.data.ctry" = pop.data.ctry, 
  "afp.data" = ctry.data$afp.all.2
) |> 
  write_rds("C:/Users/ynm2/Desktop/npafp_test_data.rds")

#start here 
library(tidyverse)
library(lubridate)
data <- read_rds("//cdc.gov/project/CGH_GID_Active/PEB/SIR/DATA/Core 2.0/test_data/npafp_test_data.rds")

f.npafp.rate.01(
  afp.data = data$afp.data, 
  pop.data = data$pop.data.dist, 
  start.date = "2019-04-01", 
  end.date = "2020-09-01", 
  spatial.scale = "prov", 
  pending = T, 
  rolling = F
)
