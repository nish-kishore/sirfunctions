run_yearly_rolling <- function(
  start.years = 2018:2021,
  anchor.date = "06-01",
  data
){

  dates <- paste0(start.years, "-", anchor.date) |> as_date()

  out1 <- tibble()
  out2 <- tibble()
  npafp <- tibble()

  for(i in 1:length(dates)){

    beginning <- dates[i]

    ending <- beginning + 364

    if(seq(beginning, ending, by = "days") |>
       as.character() |>
       endsWith("02-29") |>
       sum() > 0){
      ending <- ending + 1
    }


    x <- f.timly.detection.01(
      afp.data = data$afp.data,
      es.data = data$es.data,
      ctryseq.data = data$ctryseq.data,
      start.date = beginning,
      end.date = ending,
      rolling = T
    )

    npafp <- bind_rows(npafp, f.npafp.rate.01(
      afp.data = data$afp.data,
      pop.data = data$pop.data,
      start.date = beginning,
      end.date = ending,
      spatial.scale = "ctry",
      pending = T,
      rolling = T
    ))

    out1 <- bind_rows(out1, x$ctry)
    out2 <- bind_rows(out2, x$global)

  }

  return(list("ctry" = out1, "global" = out2, "npafp" = npafp))

}
