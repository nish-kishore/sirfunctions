#' Generate stool adequacy columns in the AFP dataset
#'
#' @param afp.data tibble of AFP data (afp.all.2)
#' @param start_date start date of the desk review
#' @param end_date end date of the desk review
#'
#' @return a tibble containing stool adequacy columns
#' @export
generate_stool_data <- function(afp.data, start_date, end_date) {

  afp.data <- afp.data |>
    filter(year >= year(start_date) & year <= year(end_date))

  stool.data <- afp.data |> # IF FUNCTION CHANGES, THIS WILL NEED TO CHANGE AS WELL
    as_tibble() |>
    filter(cdc.classification.all2 != "NOT-AFP") |>
    mutate(adequacy.final = case_when(#Conditions for Bad Data
      bad.stool1 == "data entry error" |
        bad.stool1 == "date before onset" |
        bad.stool1 == "date onset missing" ~ 77
    )) %>%
    mutate(adequacy.final = case_when(#Conditions for Bad Data
      is.na(adequacy.final)==TRUE & (bad.stool2 == "data entry error" |
                                       bad.stool2 == "date before onset" |
                                       bad.stool2 == "date onset missing") ~ 77,
      TRUE ~ adequacy.final
    )) %>%
    mutate(adequacy.final = case_when(#Conditions for Poor Adequacy
      is.na(adequacy.final)==TRUE & (ontostool1 > 13 | ontostool1 < 0 |
                                       is.na(stool1tostool2) == T |
                                       ontostool2 > 14 | ontostool2 < 1 | stool1tostool2 < 1 |
                                       stool.1.condition == "Poor" | stool.2.condition == "Poor") ~ 0,
      TRUE ~ adequacy.final)) %>%
    mutate(adequacy.final = case_when(#Conditions for Good Adequacy
      is.na(adequacy.final)==TRUE & (ontostool1 <= 13 & ontostool1 >= 0 &
                                       ontostool2 <= 14 & ontostool2 >= 1 &
                                       stool1tostool2 >= 1 & stool.1.condition == "Good" &
                                       stool.2.condition == "Good") ~ 1,
      TRUE ~ adequacy.final
    )) %>%
    mutate(adequacy.final = case_when(#Conditions for Missing Adequacy
      is.na(adequacy.final)==TRUE & (is.na(stool.1.condition) == T |
                                       is.na(stool.2.condition) == T |
                                       stool.1.condition == "Unknown" | stool.2.condition == "Unknown") ~ 99,
      TRUE ~ adequacy.final
    )) |>
    mutate(adequacy.final = case_when(
      adequacy.final == 0 ~ "Inadequate",
      adequacy.final == 1 ~ "Adequate",
      adequacy.final == 77 ~ "Bad data",
      adequacy.final == 99 ~ "Missing",
    ))|>
    mutate(adequacy.final2 = ifelse(adequacy.final == "Missing", "Adequate", adequacy.final)
    )

  return(stool.data)
}

generate_afp_prov_year <- function(afp.by.month.prov, start_date, end_date) {
  afp.month.prov.g = afp.by.month.prov |> filter(year >= year(start_date) &
                                                   year <= year(Sys.Date()))



  afp.month.prov.g$case.cat = factor(afp.month.prov.g$case.cat, levels = c(c("0", "1", "2-5", "6-9", "10+")))

  # changed to u15pop.prov instead
  # prov is not a column at afp.month.prov.g (fixed by adding to the groupby)
  # !!! address
  afp.dets.prov.year <- ggplot(
    afp.month.prov.g |>
      arrange(u15pop),
    aes(x = mon.year2,
        y = fct_inorder(prov),
        fill = case.cat)
  ) +
    geom_tile(color = "black") +
    sirfunctions::f.plot.looks("geomtile") +
    scale_fill_manual(
      values = sirfunctions::f.color.schemes("afp.prov"),
      name = "AFP Cases",
      drop = F
    ) +
    ggtitle("Number of AFP Cases by Province") +
    sirfunctions::f.plot.looks("geomtile") +
    theme(plot.caption = element_text(hjust = 0)) +
    labs(caption = "Provinces are ordered by under 15 population, with highest on top")

  return(afp.dets.prov.year)
}
