generate_ctry_timeliness_graph <- function(int.data) {
  ggplot() +
    geom_bar(
      data = int.data,
      aes(
        x = factor(year),
        y = medi,
        fill = fct_rev(type)
      ),
      position = "stack",
      stat = "identity"
    ) +
    geom_text(
      data = filter(int.data, medi != 0),
      aes(
        x = factor(year),
        y = medi,
        label = medi,
        group = fct_rev(type)
      ),
      position = position_stack(vjust = 0.5)
    ) +
    coord_flip() +
    ylab("Median Days") +
    xlab("Year of Paralysis Onset") +
    #scale_y_continuous(breaks = seq(0, max(pretty(tot.time$tot))+1)) +
    scale_x_discrete(labels = labs) +
    scale_fill_manual(
      name = "Interval",
      drop = T,
      values = f.color.schemes("timeliness.col.vars"),
      guide = guide_legend(reverse = TRUE)
    ) +
    theme(legend.position = "bottom",
          legend.background = element_blank())
}

generate_prov_timeliness_graph <- function(int.data, afp.prov.year) {
  prov.time.2 = left_join(
    int.data,
    afp.prov.year.lab,
    by = c(
      "year" = "year",
      "adm1guid" = "adm1guid",
      "prov" = "prov"
    )
  )

  timely_prov <- ggplot(prov.time.2 |>
                          filter(is.na(medi) == F & is.na(prov) == F)) +
    geom_bar(aes(
      x = as.character(labs),
      y = medi,
      fill = fct_rev(type)
    ),
    position = "stack",
    stat = "identity") +
    geom_text(aes(
      x = labs,
      y = medi,
      label = medi,
      group = fct_rev(type)
    ),
    position = position_stack(vjust = 0.5)) +
    coord_flip() +
    ylab("Median Days") +
    xlab("Year of Paralysis Onset") +
    # scale_y_continuous(breaks = seq(0, max(pretty(tot.time.p$tot))+1)) +
    scale_x_discrete() +
    ylab("Days") +
    xlab("Year") +
    scale_fill_manual(
      name = "Interval",
      values = f.color.schemes("timeliness.col.vars"),
      guide = guide_legend(reverse = TRUE),
      drop = T
    ) +
    facet_grid(prov ~ . ,
               scales = "free_y" ,
               space = "free",
               switch = "y") +
    theme(legend.position = "bottom",
          legend.background = element_blank()) +
    theme(strip.text.y = element_text(size = 5))

  return(timely_prov)
}

generate_pop_map <- function(ctry.data, prov.shape, end_date) {
  prov.pop <- ctry.data$prov.pop %>%
    filter(year == year(end_date) & ctry == country)

  # Merge with province
  shape.prov.pop <-
    left_join(prov.shape, prov.pop, by = c("GUID" = "adm1guid"))

  pop.map <- ggplot() +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    geom_sf(data = shape.prov.pop,
            aes(fill = u15pop)) +
    geom_sf(data = st_crop(ctry.data$roads, ctry.data$ctry)) +
    geom_sf(
      data = filter(ctry.data$cities, toupper(CNTRY_NAME) == ctry.data$name),
      size = 3,
      color = "blue"
    ) +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 15
    ) +
    geom_sf_label_repel(data = filter(ctry.data$cities,
                                      toupper(CNTRY_NAME) == ctry.data$name),
                        aes(label = CITY_NAME)) +
    scale_fill_distiller(palette = "YlOrRd",
                         direction = 1,
                         labels = scales::comma) +
    ggtitle(paste0(
      "Major Cities and Roads - Province Level Population - ",
      year(end_date)
    )) +
    labs(fill = "Under-15 pop") +
    sirfunctions::f.plot.looks("epicurve") +
    scale_size_identity() +
    labs(caption = "- Under 15 population is shown at the province level\n- Major roads are shown in black\n- Population centers are shown in blue") +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      plot.caption = element_text(hjust = 0, size = 11),
      legend.background = element_blank()
    )

  return(pop.map)
}

generate_dist_pop_map <- function(ctry.data, prov.shape, dist.shape, end_date) {
  prov.pop <- ctry.data$prov.pop %>%
    filter(year == year(end_date) & ctry == country)

  dist.pop <- ctry.data$dist.pop %>%
    filter(year == year(end_date) & ctry == country)

  shape.prov.pop <-
    left_join(prov.shape, prov.pop, by = c("GUID" = "adm1guid"))

  shape.dist.pop <-
    left_join(dist.shape, dist.pop, by = c("GUID" = "adm2guid"))


  pop.map.provn <- ggplot() +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    geom_sf(data = shape.dist.pop,
            aes(fill = u15pop), color = NA) +
    geom_sf(data = prov.shape,
            color = "black",
            fill = NA) +
    geom_sf_label_repel(data = shape.prov.pop,
                        aes(label = ADM1_NAME), force = 80) +
    scale_fill_distiller(palette = "YlOrRd",
                         direction = "both",
                         labels = scales::comma) +
    ggtitle(paste0("Province Names - District Level Population - ", year(end_date))) +
    labs(fill = "Under-15 pop") +
    sirfunctions::f.plot.looks("epicurve") +
    scale_size_identity() +
    labs(caption = "- Under 15 population is shown at the district level\n- Labels are province names\n- Black lines are province borders") +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      plot.caption = element_text(hjust = 0, size = 11),
      legend.background = element_blank()
    )

  return(pop.map.provn)
}

generate_afp_case_map <- function(ctry.data, ctry.shape, prov.shape, start_date, end_date) {
  afp.case.map.filter <- ctry.data$afp.all %>%
    filter(between(as.Date(date.onset), start_date, end_date)) |>
    mutate(year = as.factor(year))

  afp.case.map <- ggplot() +
    geom_sf(data = ctry.shape, color = "black", fill = NA, size = 1) +
    geom_sf(data = prov.shape, color = "black", fill = NA, size = .5) +
    geom_sf(
      data = afp.case.map.filter |>
        filter(!(cdc.class %in% c("PENDING", "NPAFP", "UNKNOWN", "NOT-AFP", "LAB PENDING"))),
      aes(color = cdc.classification.all2), size = 1
    ) +
    scale_color_manual(
      values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
      drop = F
    ) +
    ggtitle(paste("Paralytic Polio and Compatible Cases",
                  year(start_date), "-", year(end_date))) +
    # NOTE: IF THERE ARE NONE IT NEEDS TO THROW AN ERROR
    sirfunctions::f.plot.looks("epicurve") +
    facet_wrap(~year, ncol = 4) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )

  return(afp.case.map)

}

generate_afp_epicurve <- function(ctry.data, start_date, end_date) {
  afp.epi.date.filter <- ctry.data$afp.epi %>%
    filter(between(yronset, as.numeric(year(start_date)), as.numeric(year(end_date))))

  case.num.labs <- reframe(group_by(afp.epi.date.filter, yronset),
                           labs = paste0(yronset, " (N = ", sum(afp.cases), ")")) %>%
    distinct(.)

  afp.epi.date.filter1 <- left_join(afp.epi.date.filter,
                                    case.num.labs,
                                    by = c("yronset" = "yronset"))

  afp.epi.curve <- ggplot(afp.epi.date.filter1,
                           aes(fill = cdc.classification.all2,
                               y = afp.cases, x = epi.week)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual(
      values = sirfunctions::f.color.schemes(type = "epicurve"),
      name = "Classification",
      drop = T
    ) +
    sirfunctions::f.plot.looks(type = "epicurve") +
    facet_wrap( ~ labs, ncol = 3, drop = F)

  return(afp.epi.curve)
}

generate_iss_barplot <- function(iss.data, start_date, end_date) {

  issy2 <- iss.data |>
    mutate(today_date = as_date(today, format = "%m/%d/%Y"),
           date_of_visit = as_date(date_of_visit, format = "%m/%d/%Y"))

  issy2.1 = issy2 %>%
    filter(today_date<=Sys.Date() &
             today_date>= start_date)

  count(issy2, issy2$year<=as.yearmon(Sys.Date()))

  issy3 = issy2.1 %>%
    #filter(is_priority_afp=="Yes") %>%
    group_by(month, year, priority_level) %>%
    summarize(freq = n()) %>%
    filter(year<=year(Sys.Date()) &
             year>= year(start_date))

  issy3$labs = month.abb[issy3$month] %>%
    factor(., levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                         "Aug", "Sep", "Oct", "Nov", "Dec"))
  issy3 <- issy3 |>
    filter(between(year, year(start_date), year(end_date)))
  # red = "#d73027"
  # orange = "#fdae61"
  # blue = #4575b4
  # grey = #878787

  totty = issy3 %>%
    group_by(year, month) %>%
    summarize(totty = sum(freq))

  mtot = max(totty$totty)


  issy.vis = ggplot(data = issy3) +
    geom_bar(aes(x = factor(labs), y = freq,
                 fill = priority_level), stat = "identity",
             position = "stack", col = "black")+
    scale_y_continuous(name = "Visit Number", limits = c(0, max(pretty(mtot))),
                       breaks = seq(0, max(pretty(mtot)), max(pretty(mtot))/5),
                       labels = seq(0, max(pretty(mtot)), max(pretty(mtot))/5))+
    scale_x_discrete(name = "Time")+
    scale_fill_manual(name = "Priority",
                      values = c(
                        "High" = "#d73027",
                        "Medium" = "#fdae61",
                        "Low" = "#4575b4",
                        "Not Focal Site" = "#878787"

                      ))+
    facet_wrap(~year) +
    theme_bw()

  return(issy.vis)
}

generate_iss_map <- function(iss.data, start_date, end_date) {
  issy2 <- iss.data |>
    mutate(today_date = as_date(today, format = "%m/%d/%Y"),
           date_of_visit = as_date(date_of_visit, format = "%m/%d/%Y"))

  pryr = count(issy2, priority_level,year) %>%
    filter(priority_level == "High")

  issy4 = full_join(issy2, pryr)

  issy4$labs = paste0(issy2$year, "\n(n = ",issy4$n,")")

  issy.map = ggplot()+
    geom_sf(data = prov.shape, color = "black", fill = NA, size = .5) +
    geom_point(data = issy4|>
                 filter(year<=year(end_date) &
                          year>= year(start_date) &
                          priority_level=="High"),
               aes(x = as.numeric(`_gps_ending_longitude`),
                   y = as.numeric(`_gps_ending_latitude`),
                   col = priority_level))+
    sirfunctions::f.plot.looks("epicurve") +
    scale_color_manual("Priority level", values = c("High" = "#d73027"))+
    facet_wrap(~labs, ncol=4) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )

  return(issy.map)
}

generate_surv_ind_tab <- function(ctry.data, dis.extract, dist.ind.afp, cstool, dstool, afp.case) {
  dist.ind.afp <- left_join(dis.extract,
                            dstool,
                            by = c(
                              "prov" = "prov",
                              "dist" = "dist",
                              "year" = "year"
                            ))


  # population meeting both >=2 NPAFP rate and >=80% stool adequacy
  tot.dist.pop <- dist.ind.afp %>%
    group_by(year) %>%
    summarize(tot.dist.pop = sum(u15pop, na.rm = T))

  dist.adeq.ind <- dist.ind.afp %>%
    filter(npafp_rate >= 2 & per.stool.ad >= 80) %>%
    group_by(year) %>%
    summarize(tot.dist.adeq = sum(u15pop, na.rm = T))

  meet.ind <- left_join(tot.dist.pop, dist.adeq.ind,
                        by = c("year" = "year")) %>%
    mutate(across(tot.dist.adeq, ~ replace_na(.x, 0)))

  meet.ind$prop.dist.adeq <-
    100 * meet.ind$tot.dist.adeq / meet.ind$tot.dist.pop

  ctry.ind.afp <- left_join(ctry.extract, cstool,
                            by = c("year", "adm0guid"))
  ctry.ind.afp <- left_join(ctry.ind.afp, afp.case,
                            by = c("year"))
  temp.ind.tab <- left_join(ctry.ind.afp, meet.ind, by = c("year"))

  ## Districts with population over 100K
  dist.100k <- dis.extract %>%
    filter(u15pop >= 100000)

  unique.dist.100k <- ctry.data$dist.pop %>%
    filter(ctry == country & u15pop > 100000) %>%
    unique() %>%
    group_by(year, u15pop, adm2guid) %>%
    filter(u15pop > 100000) %>%
    filter(year >= year(start_date) &
             year <= year(end_date))

  num.dists.100k <- unique.dist.100k %>%
    group_by(year) %>%
    summarize(dist.100k.num = n()) # total number of districts with pop >100K u15


  ad.dists.100k <- left_join(dstool, dist.100k, by = c("year" = "year",
                                                       "adm2guid" = "adm2guid")) %>%
    filter(npafp_rate >= 2 & per.stool.ad >= 80) %>%
    group_by(year) %>%
    summarize(ad.dist.100k.num = n())
  # number of dists meeting both requirements with pop >100K u15

  adeq.dists <-
    left_join(num.dists.100k, ad.dists.100k, by = c("year")) %>%
    mutate(across(everything(), ~ replace_na(.x, 0))) %>%
    mutate(prop = paste0(ad.dist.100k.num, "/", dist.100k.num))

  temp.ind.tab1 <-
    left_join(temp.ind.tab, adeq.dists, by = c("year" = "year"))

  ## Making a flextable for this
  temp.ind.tab2 <- temp.ind.tab1 %>%
    mutate(across(
      c(afp.cases, npafp_rate, per.stool.ad, prop.dist.adeq),
      ~ as.numeric(.)
    )) %>%
    mutate(across(
      c(afp.cases, npafp_rate, per.stool.ad, prop.dist.adeq),
      ~ round(., 1)
    ))

  temp.ind.tab.flex <- as.data.frame(t(temp.ind.tab2)) %>%
    row_to_names(row_number = 1) %>%
    rownames_to_column("type") %>%
    filter(type %in% c(
      "afp.cases",
      "npafp_rate",
      "per.stool.ad",
      "prop.dist.adeq",
      "prop"
    )) %>%
    mutate(
      type = case_when(
        type == "npafp_rate" ~ "NPAFP rate*",
        type == "afp.cases" ~ "AFP cases",
        type == "per.stool.ad" ~ "Stool adequacy**",
        type == "prop.dist.adeq" ~ "Population U15 living in districts that met both indicators",
        type == "prop" ~ "Districts >= 100,000 U15 that met both indicators",
        FALSE ~ type
      )
    )

  temp.ind.tab.flex <-
    temp.ind.tab.flex[c(2, 1, 3, 4, 5),] # Reorder the table to be in the correct order

  ## Flextable of surveillance indicators (surv.ind.tab)
  surv.ind.tab <- flextable(temp.ind.tab.flex) %>%
    theme_booktabs() %>%
    bold(bold = TRUE, part = "header") %>%
    colformat_double(
      j = 2:ncol(temp.ind.tab.flex),
      digits = 1,
      na_str = "---"
    ) %>%
    set_header_labels(type = "") %>%
    add_footer_row(
      top = F,
      "*Stool adequacy defined as per Certification Indicator, i.e., 2 stools collected at least 24h apart AND ≤14d of onset AND received in good condition at a WHO-accredited laboratory (missing condition assumed good)",
      colwidths = ncol(temp.ind.tab.flex)
    ) %>%
    autofit()

  return(surv.ind.tab)
}

generate_pop_tab <- function(prov.case.ind, pstool, start_date, end_date) {

  sub.prov.case.ind = prov.case.ind %>%
    select(year, n_npafp, u15pop, prov, npafp_rate)

  # only 6 provinces
  sub.pstool = pstool %>%
    select(year, per.stool.ad, prov) |>
    filter(!is.na(prov))

  sub.prov.join = full_join(sub.prov.case.ind, sub.pstool, by = c("year", "prov")) %>%
    arrange(prov, year)

  sub.prov.join = sub.prov.join %>%
    group_by(prov) %>%
    mutate(diff = lag(n_npafp)) %>%
    mutate(diff_per = round(100*(n_npafp - lag(n_npafp))/lag(n_npafp), 1)) %>%
    mutate(across(c(per.stool.ad, diff, diff_per, n_npafp),round, 0)) %>%
    mutate(across(c(npafp_rate), round, 1)) |>
    filter(!is.na(prov))

  sub.prov.join

  date.analysis =  seq(year(start_date), year(end_date),1)
  pop.date.analysis =paste0("u15pop_", date.analysis[1:length(date.analysis)-1])

  sub.prov.join.wide = pivot_wider(sub.prov.join, names_from = year,
                                   values_from = c(per.stool.ad, diff, diff_per,
                                                   n_npafp, npafp_rate, u15pop))%>%
    select(-all_of(pop.date.analysis))

  var.ord = c("prov", paste0("u15pop_", date.analysis[length(date.analysis)]),
              paste0("n_npafp_", date.analysis), paste0("diff_per_",
                                                        date.analysis[2:length(date.analysis)]),
              paste0("npafp_rate_", date.analysis),
              paste0("per.stool.ad_", date.analysis))

  sub.prov.join.wide = sub.prov.join.wide[,c(var.ord)] %>%
    replace(is.na(.), 0)

  var.ord.case = c("prov", paste0("u15pop_", date.analysis[length(date.analysis)]),
                   paste0("n_npafp_", date.analysis), paste0("diff_per_",
                                                             date.analysis[2:length(date.analysis)]))

  # NPAFP table
  col_palette <- c("#FF9999", "white")
  col.npafp.rate =  sub.prov.join.wide[,c(paste0("npafp_rate_", date.analysis))] %>%
    mutate(across(everything(),~ replace_na(.x, 0)))%>%
    mutate(across(c(everything()), cut, breaks = c(0,2),
                  right = F, label = FALSE))

  npafp.rate.colors <- col_palette[as.matrix(col.npafp.rate)]

  # Stool adequacy
  col_palette <- c("#FF9999", "white")
  col.stool.ad =  sub.prov.join.wide[,c(paste0("per.stool.ad_", date.analysis))] %>%
    mutate(across(everything(),~ replace_na(.x, 0)))%>%
    mutate(across(c(everything()), cut, breaks = c(0,80),
                  right = F, label = FALSE))

  stool.ad.colors <- col_palette[as.matrix(col.stool.ad)]

  # case vars only
  sub.prov.join.wide.case = sub.prov.join.wide %>%
    select(all_of(var.ord.case))
  # Cases and differences

  null.col = rep(c(NA),times=ncol(sub.prov.join.wide.case)*nrow(sub.prov.join.wide.case))

  col.mat = c(null.col, npafp.rate.colors, stool.ad.colors)

  # Make provinces not meeting indicators red
  # If stool ad or NPAFP below threshold - color = "#CC0000"
  # Subset of prov not meeting indicators any year
  inad.prov = sub.prov.join %>%
    filter(npafp_rate<2|per.stool.ad<80)

  uni.inad.prov = match(unique(inad.prov$prov), sub.prov.join.wide$prov)

  # Color matrix
  col.mat.txt = col.mat %>%
    str_replace(., "#FF9999","#CC0000" )
  col.mat.txt[uni.inad.prov] = "#CC0000"

  # Flextable column formatting calculations
  # # NPAFP cases length
  npafp.case.length = length(subset(var.ord, grepl("n_n", var.ord) ==T |
                                      grepl("diff", var.ord)==T))
  # NPAFP rate length
  npafp.rate.length = length(subset(var.ord, grepl("rate", var.ord) ==T))
  # stool adequacy length
  stool.ad.length = length(subset(var.ord, grepl("stool", var.ord) ==T))

  # Labels for % difference
  diff.yr = length(which(grepl("diff", names(sub.prov.join.wide)) ==T))

  diff.lab = NULL
  for(i in 1:(diff.yr)){
    diff.lab[i] = paste("% difference ", min(date.analysis)+i-1, "-",
                        min(date.analysis)+i)
  }

  # Names for flextable columns
  names1 = names(sub.prov.join.wide)
  names2 = c("Province", paste0("U15 Population - ", max(date.analysis)),
             date.analysis, diff.lab,
             date.analysis, date.analysis)


  small_border = fp_border_default(color="black", width = 1)
  # pop.tab flextable
  pop.tab = flextable(sub.prov.join.wide) %>%
    theme_booktabs() %>%
    bg(j = colnames(sub.prov.join.wide), bg = col.mat) %>%
    color(j = colnames(sub.prov.join.wide), col = col.mat.txt) %>%
    align(align = "center", part = "all") %>%
    set_header_df(mapping = data.frame(keys = names1,
                                       values = names2,
                                       stringsAsFactors = FALSE),
                  key = "keys" ) %>%
    add_header_row(values = c("", "# NP AFP Cases", "NP AFP rate", "% Stool Adequacy"),
                   colwidths = c(2, npafp.case.length, stool.ad.length,
                                 stool.ad.length), top = TRUE) %>%
    vline(j = c(2,2+npafp.case.length,2+npafp.case.length+stool.ad.length),
          border = small_border)%>%
    hline(part = "header") %>%
    bold(bold = TRUE, part = "header") %>%
    align(align = "center", part = "all")

  return(pop.tab)
}

generate_npafp_maps <- function(prov.extract, ctry.shape, prov.shape, start_date, end_date) {
  provnpafp <- prov.extract

  provnpafp$cats <- cut(
    provnpafp$npafp_rate,
    breaks = c(-1, 0, 1, 2, 3, 1000),
    right = F,
    labels = c("Zero NPAFP cases", "<1",
               "1-<2", "2-<3", "3+")
  )

  prov.cut <- provnpafp %>%
    mutate(cats = as.character(cats)) %>%
    mutate(
      cats = case_when(
        npafp_rate == 0 & u15pop >= 100000 ~ "Silent (u15pop >= 100K)",
        npafp_rate == 0 &
          u15pop < 100000 & u15pop > 0 ~ "No cases (u15pop < 100K)",
        npafp_rate == 0 & u15pop == 0 ~ "Missing Pop",
        T ~ cats
      )
    ) %>%
    filter(year >= year(start_date) & year <= year(end_date))


  prov.cut$cats <- factor(
    prov.cut$cats,
    levels = c(
      "<1" = "<1",
      "1-<2" = "1-<2",
      "2-<3" = "2-<3",
      "3+" = "3+",
      "Missing Pop" = "Missing Pop",
      "No cases (u15pop < 100K)" = "No cases (u15pop < 100K)",
      "Silent (u15pop >= 100K)" = "Silent (u15pop >= 100K)"
    )
  )

  prov.cut = ungroup(prov.cut)

  # For those with no cases --> separate out districts with u15pop >100K and <100K

  prov.pop.case.npafp <-
    full_join(prov.shape, prov.cut, by = c("GUID" = "adm1guid")) %>%
    filter(year <= year(end_date) & year >= year(start_date))

  # Labels for provinces meeting NPAFP rate
  # How many provinces meet >2 NPAFP?
  prov.2npafp = provnpafp %>%
    group_by(year, adm1guid, prov) %>%
    summarize(meet2 = sum(npafp_rate >= 2, na.rm = T)) %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(num.meet2 = sum(meet2, na.rm = T),
              len.year = length(year)) %>%
    mutate(
      labs = paste0(
        num.meet2,
        "/",
        len.year,
        " (",
        round(100 * num.meet2 / len.year, 0),
        "%)",
        " provinces \nwith >= 2 cases of NPAFP \nper 100,000 population"
      )
    )


  # Get coordinates for maps that are plotted
  ctcoord = as.data.frame(st_coordinates(ctry.shape))
  # Put text at 10% below the minimum X and Y coords for each map
  adjy = (range(ctcoord$Y)[1] - range(ctcoord$Y)[2]) * .1


  npafp.maps <- ggplot() +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    geom_sf(data = prov.pop.case.npafp, color = "black", aes(fill = cats)) +
    geom_text(
      data = prov.2npafp,
      aes(
        x = min(ctcoord$X),
        y = min(ctcoord$Y) + adjy,
        label = labs
      ),
      size = 3,
      check_overlap = TRUE,
      hjust = 0
    ) +
    scale_fill_manual(
      name = "NPAFP rate",
      values = c(
        "No cases (u15pop < 100K)" = "lightgrey",
        "<1" = "#d7191c",
        "1-<2" = "#fdae61",
        "2-<3" = "#a6d96a",
        "3+" = "#1a9641",
        "Missing Pop" = "#2C83C7",
        "Silent (u15pop >= 100K)" = "#5e3c99"
      ),
      drop = F
    ) +
    ggtitle("NPAFP Rate Annualized - Province") +
    sirfunctions::f.plot.looks("epicurve") +
    facet_wrap( ~ year, ncol = 4) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )

  return(npafp.maps)

}

generate_npafp_maps_dist <- function(dist.extract, ctry.shape, prov.shape, dist.shape, start_date, end_date) {
  distnpafp <- dis.extract

  distnpafp$cats <- cut(
    distnpafp$npafp_rate,
    breaks = c(-1, 0, 1, 2, 3, 1000),
    right = F,
    labels = c("Zero NPAFP cases", "<1",
               "1-<2", "2-<3", "3+")
  )

  # For those with no cases --> separate out districts with u15pop >100K and <100K

  dist.cut <- distnpafp %>%
    mutate(cats = as.character(cats)) %>%
    mutate(
      cats = case_when(
        npafp_rate == 0 & u15pop >= 100000 ~ "Silent (u15pop >= 100K)",
        npafp_rate == 0 &
          u15pop < 100000 & u15pop > 0 ~ "No cases (u15pop < 100K)",
        npafp_rate == 0 & u15pop == 0 ~ "Missing Pop",
        T ~ cats
      )
    ) %>%
    filter(year >= year(start_date) & year <= year(end_date))


  dist.cut$cats <- factor(
    dist.cut$cats,
    levels = c(
      "<1" = "<1",
      "1-<2" = "1-<2",
      "2-<3" = "2-<3",
      "3+" = "3+",
      "Missing Pop" = "Missing Pop",
      "No cases (u15pop < 100K)" = "No cases (u15pop < 100K)",
      "Silent (u15pop >= 100K)" = "Silent (u15pop >= 100K)"
    )
  )

  dist.cut = ungroup(dist.cut)

  dist.pop.case.npafp <-
    left_join(dist.shape, dist.cut, by = c("GUID" = "adm2guid"))

  # How many districts meet >2 NPAFP?
  dist.2npafp = distnpafp %>%
    group_by(year, adm2guid, dist) %>%
    summarize(meet2 = sum(npafp_rate >= 2, na.rm = T)) %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(num.meet2 = sum(meet2, na.rm = T),
              len.year = length(year)) %>%
    mutate(
      labs = paste0(
        num.meet2,
        "/",
        len.year,
        " (",
        round(100 * num.meet2 / len.year, 0),
        "%)",
        " districts \nwith >= 2 cases of NPAFP \nper 100,000 population"
      )
    )


  # Get coordinates for maps that are plotted
  ctcoord = as.data.frame(st_coordinates(ctry.shape))
  # Put text at 10% below the minimum X and Y coords for each map
  adjy = (range(ctcoord$Y)[1] - range(ctcoord$Y)[2]) * .1

  npafp.maps.dist <- ggplot() +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    #geom_sf(data = ctry.data$dist, color = "black", fill = "lightgrey", size = .5) +
    geom_sf(data = dist.pop.case.npafp, color = "black", aes(fill = cats)) +
    geom_text(
      data = dist.2npafp,
      aes(
        x = min(ctcoord$X),
        y = min(ctcoord$Y) + adjy,
        label = labs
      ),
      size = 3,
      check_overlap = TRUE,
      hjust = 0
    ) +
    scale_fill_manual(
      name = "NPAFP rate",
      values = c(
        "No cases (u15pop < 100K)" = "lightgrey",
        "<1" = "#d7191c",
        "1-<2" = "#fdae61",
        "2-<3" = "#a6d96a",
        "3+" = "#1a9641",
        "Missing Pop" = "#2C83C7",
        "Silent (u15pop >= 100K)" = "#5e3c99"
      ),
      drop = F
    ) +
    # scale_color_manual(values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
    #                  drop = F) +
    ggtitle("NPAFP Rate Annualized - District") +
    sirfunctions::f.plot.looks("epicurve") +
    facet_wrap( ~ year, ncol = 4) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  return(npafp.maps.dist)
}

generate_stool_ad_maps <- function(ctry.data, pstool, ctry.shape, prov.shape, start_date, end_date) {
  allafp = ctry.data$afp.all.2 %>%
    filter(date >= start_date & date <= end_date) %>%
    reframe(group_by(ctry.data$afp.all.2, cdc.classification.all2,
                     adm1guid, year),
            freq = n()) %>%
    filter(cdc.classification.all2 != "NOT-AFP")

  allprov = ctry.data$prov.pop[, c("adm1guid", "year", "prov")] %>%
    filter(year >= year(start_date) & year <= year(end_date))

  all.prov.afp = left_join(allprov, allafp) %>%
    group_by(year, adm1guid, prov) %>%
    summarize(allafp = sum(freq, na.rm = T))

  stoolad.p = left_join(all.prov.afp,
                        pstool,
                        by = c(
                          "prov" = "prov",
                          "year" = "year",
                          "adm1guid" = "adm1guid"
                        ))

  stoolad.p = stoolad.p %>%
    tibble() %>%
    mutate(
      prop.cat = case_when(
        allafp == 0 ~ "Zero AFP cases",
        allafp != 0 & per.stool.ad < 40 ~ "<40%",
        allafp != 0 &
          per.stool.ad >= 40 & per.stool.ad < 60 ~ "40-59%",
        allafp != 0 &
          per.stool.ad >= 60 & per.stool.ad < 80 ~  "60-79%",
        allafp != 0 & per.stool.ad >= 80 ~  "80%+"
      )
    ) %>%
    mutate(prop.cat = factor(
      prop.cat,
      levels = c("Zero AFP cases", "<40%", "40-59%",
                 "60-79%", "80%+")
    ))

  stoolad.nums.p = stoolad.p %>%
    group_by(year, adm1guid, prov) %>%
    summarize(meet.stool = sum(per.stool.ad >= 80)) %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(num.meet.stool = sum(meet.stool),
              len.year = length(year)) %>%
    mutate(
      labs = paste0(
        num.meet.stool,
        "/",
        len.year,
        " (",
        round(100 * num.meet.stool / len.year, 0),
        "%)",
        " provinces with >= 80% stool adequacy"
      )
    )

  stool.map.p <-
    left_join(prov.shape, stoolad.p, by = c("GUID" = "adm1guid"))

  # Get coordinates for maps that are plotted
  ctcoord = as.data.frame(st_coordinates(ctry.shape))
  # Put text at 10% below the minimum X and Y coords for each map
  adjy = (range(ctcoord$Y)[1] - range(ctcoord$Y)[2]) * .1


  stool.ad.maps <- ggplot() +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    geom_sf(data = stool.map.p, color = "black", aes(fill = prop.cat)) +
    geom_text(
      data = stoolad.nums.p,
      aes(
        x = min(ctcoord$X),
        y = min(ctcoord$Y) + adjy,
        label = labs
      ),
      check_overlap = TRUE,
      hjust = 0
    ) +
    scale_fill_manual(
      name = "Stool Adequacy",
      values = c(
        "Zero AFP cases" = "lightgrey",
        "<40%" = "#fdae61",
        "40-59%" = "#ffffbf",
        "60-79%" = "#abd9e9",
        "80%+" = "#2c7bb6",
        "Unable to Assess" = "white"
      ),
      drop = F
    ) +
    ggtitle("Stool Adequacy - Province") +
    sirfunctions::f.plot.looks("epicurve") +
    facet_wrap( ~ year, ncol = 4) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  return(stool.ad.maps)
}

generate_stool_ad_maps_dist <- function(ctry.data, dstool,ctry.shape, dist.shape, prov.shape, start_date, end_date) {
  # Get coordinates for maps that are plotted
  ctcoord <- as.data.frame(st_coordinates(ctry.shape))
  # Put text at 10% below the minimum X and Y coords for each map
  adjy <- (range(ctcoord$Y)[1] - range(ctcoord$Y)[2])*.1

  allafp.d = ctry.data$afp.all.2 %>%
    filter(date >= start_date & date <= end_date) %>%
    reframe(group_by(ctry.data$afp.all.2, cdc.classification.all2,
                     adm2guid, year),
            freq = n()) %>%
    filter(cdc.classification.all2 != "NOT-AFP")

  alldist = ctry.data$dist.pop[, c("adm2guid", "year", "prov", "dist")] %>%
    filter(year >= year(start_date) & year <= year(end_date))

  all.dist.afp = left_join(alldist, allafp.d) %>%
    group_by(year, adm2guid, prov, dist) %>%
    summarize(allafp = sum(freq, na.rm = T))

  stoolad.d = left_join(
    all.dist.afp,
    dstool,
    by = c(
      "prov" = "prov",
      "year" = "year",
      "adm2guid" = "adm2guid",
      "dist" = "dist"
    )
  )

  stoolad.d = stoolad.d %>%
    tibble() %>%
    mutate(
      prop.cat = case_when(
        allafp == 0 ~ "Zero AFP cases",
        allafp != 0 & per.stool.ad < 40 ~ "<40%",
        allafp != 0 &
          per.stool.ad >= 40 & per.stool.ad < 60 ~ "40-59%",
        allafp != 0 &
          per.stool.ad >= 60 & per.stool.ad < 80 ~  "60-79%",
        allafp != 0 & per.stool.ad >= 80 ~  "80%+"
      )
    ) %>%
    mutate(prop.cat = factor(
      prop.cat,
      levels = c("Zero AFP cases", "<40%", "40-59%", "60-79%", "80%+")
    ))

  stoolad.nums.d = stoolad.d %>%
    group_by(year, adm2guid, dist) %>%
    summarize(meet.stool = sum(per.stool.ad >= 80)) %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(num.meet.stool = sum(meet.stool),
              len.year = length(year)) %>%
    mutate(
      labs = paste0(
        num.meet.stool,
        "/",
        len.year,
        " (",
        round(100 * num.meet.stool / len.year, 0),
        "%)",
        " districts with >= 80% stool adequacy"
      )
    )

  stool.map.d <-
    left_join(dist.shape, stoolad.d, by = c("GUID" = "adm2guid"))

  stool.ad.maps.dist <- ggplot() +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    geom_sf(
      data = dist.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    geom_sf(data = stool.map.d, color = "black", aes(fill = prop.cat)) +
    geom_text(
      data = stoolad.nums.d,
      aes(
        x = min(ctcoord$X),
        y = min(ctcoord$Y) + adjy,
        label = labs
      ),
      check_overlap = TRUE,
      hjust = 0
    ) +
    scale_fill_manual(
      name = "Stool Adequacy",
      values = c(
        "Zero AFP cases" = "lightgrey",
        "<40%" = "#fdae61",
        "40-59%" = "#ffffbf",
        "60-79%" = "#abd9e9",
        "80%+" = "#2c7bb6",
        "Unable to Assess" = "white"
      ),
      drop = F
    ) +
    ggtitle("Stool Adequacy - District") +
    sirfunctions::f.plot.looks("epicurve") +
    facet_wrap( ~ year, ncol = 4) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  return(stool.ad.maps.dist)
}

generate_inad_tab <- function(ctry.data, stool.data, cstool, start_date, end_date) {
  stool.sub = cstool[, c("year",
                         "num.adj.w.miss",
                         "num.inadequate",
                         "per.stool.ad",
                         "afp.cases")]

  # Late collection (%) among inadequate
  inads <- stool.data %>%
    filter(date >= start_date & date <= end_date) %>%
    filter(adequacy.final == "Inadequate")

  # Timeliness
  late.inads = inads %>%
    mutate(
      timelystool = case_when(
        ontostool1 > 13 |
          ontostool1 < 0 |
          is.na(stool1tostool2) == T |
          ontostool2 > 14 |
          ontostool2 < 1 |
          stool1tostool2 < 1 ~ "Not Timely",
        T ~ "Timely"
      )
    )


  late.stool = count(late.inads, year, timelystool) %>%
    filter(timelystool == "Not Timely")
  # variables exclude bad dates as 77 or Unable to Assess
  # Missing dates treated as absence of collection of stool as there is no variable
  # that specifies stool was not collected

  # No stool/one stool/one stool among inadequate
  count(inads, stoolmissing, stool1missing, stool2missing)

  stool.miss.any = summarize(
    group_by(inads, year),
    stoolmiss = sum(stoolmissing, stool1missing,
                    stool2missing, na.rm = T)
  )


  # Poor condition among inadequate
  count(inads, stool.1.condition, stool.2.condition)

  cond.poor = filter(inads, stool.1.condition == "Poor" |
                       stool.2.condition == "Poor")

  yrs = as.data.frame(seq(year(start_date), year(end_date), 1))
  names(yrs) = "year"

  cond.poor.num = count(cond.poor, year)
  cond.poor.num = left_join(yrs, cond.poor.num, by = c("year" = "year")) %>%
    mutate(across(c(n), ~ replace_na(.x, 0)))

  # ALL AFP
  afps.all = ctry.data$afp.all.2 %>%
    filter(date >= start_date & date <= end_date)

  # 1 stool within 14 days of onset (+condition)
  good.cond.1 = count(
    afps.all,
    (ontostool1 <= 14 | ontostool2 <= 14) &
      (stool.1.condition == "Good" |
         is.na(stool.1.condition)) &
      (stool.2.condition == "Good" |
         is.na(stool.2.condition)),
    year
  )

  colnames(good.cond.1)[1] = "conds"
  good.cond.1 = good.cond.1 %>%
    filter(conds == TRUE)
  # 2 stools within 21 days of onset (+condition)
  good.cond.2 = count(
    afps.all,
    stool1missing == 0 &
      stool2missing == 0 &
      ontostool2 <= 21 &
      (stool.1.condition == "Good" |
         is.na(stool.1.condition)) &
      (stool.2.condition == "Good" |
         is.na(stool.2.condition)),
    year
  )
  colnames(good.cond.2)[1] = "conds"
  good.cond.2 = good.cond.2 %>%
    filter(conds == TRUE)

  # Time to lab
  # !!! daysstooltolab is not a variable in afp.all.2, had to recreate it
  medi_lab = summarize(
    group_by(ctry.data$afp.all.2, year),
    medi = median(ctry.data$afp.all.2$daysstooltolab, na.rm = T)
  )

  # Bind together tables
  allinadstool = left_join(stool.sub, late.stool, by = "year") %>%
    select(-timelystool) %>%
    rename("timelystool" = "n") %>%
    left_join(stool.miss.any, by = "year") %>%
    left_join(cond.poor.num, by = "year") %>%
    rename("cond.poor.num" = "n") %>%
    left_join(good.cond.1, by = "year") %>%
    select(-conds) %>%
    rename("good.cond.1" = "n") %>%
    left_join(good.cond.2, by = "year") %>%
    select(-conds) %>%
    rename("good.cond.2" = "n")

  allinadstool$timelyper = paste0(
    allinadstool$timelystool,
    " (",
    round(100 * allinadstool$timelystool / allinadstool$num.inadequate, 0),
    "%)"
  )
  allinadstool$poorper = paste0(
    allinadstool$cond.poor.num,
    " (",
    round(
      100 * allinadstool$cond.poor.num / allinadstool$num.inadequate,
      0
    ),
    "%)"
  )
  allinadstool$missingper = paste0(
    allinadstool$stoolmiss,
    " (",
    round(100 * allinadstool$stoolmiss / allinadstool$num.inadequate, 0),
    "%)"
  )
  allinadstool$good.cond.1per = paste0(
    round(100 * allinadstool$good.cond.1 / allinadstool$afp.cases, 0),
    "% (",
    allinadstool$good.cond.1,
    "/",
    allinadstool$afp.cases,
    " cases)"
  )
  allinadstool$good.cond.2per = paste0(
    round(100 * allinadstool$good.cond.2 / allinadstool$afp.cases, 0),
    "% (",
    allinadstool$good.cond.2,
    "/",
    allinadstool$afp.cases,
    " cases)"
  )

  allinadstool$per.stool.ad = round(allinadstool$per.stool.ad, 1)


  #Among inadequate cases
  #Among all AFP cases
  #Transport to the lab (median days)
  #NPENT (%)


  inad.tab = as.data.frame(t(allinadstool)) %>%
    row_to_names(row_number = 1) %>%
    rownames_to_column("type") %>%
    filter(
      type %in% c(
        "num.adj.w.miss",
        "num.inadequate",
        "per.stool.ad",
        "timelyper",
        "missingper",
        "poorper",
        "good.cond.1per",
        "good.cond.2per"
      )
    ) %>%
    mutate(
      type = case_when(
        type == "num.adj.w.miss" ~ "Cases with adequate stools",
        type == "num.inadequate" ~ "Cases with inadequate stools",
        type == "per.stool.ad" ~ "Stool adequacy*",
        type == "afp.cases" ~ "",
        type == "timelyper" ~ "Late collection (%)",
        type == "missingper" ~ "No Stool/one stool",
        type == "poorper" ~ "Poor condition",
        type == "good.cond.1per" ~ "1 stool within 14 days of onset (+ condition)",
        type == "good.cond.2per" ~ "2 stools within 21 days of onset (+ condition)",
        FALSE ~ type
      )
    )

  inad.tab <-
    inad.tab[c(3, 1, 2, 4, 6, 5, 7, 8),] # Reorder the table to be in the correct order

  inad.tab$sub = c(
    "",
    "",
    "",
    "Among Inadequate Cases",
    "Among Inadequate Cases",
    "Among Inadequate Cases",
    "Among All Cases",
    "Among All Cases"
  )

  inad.tab.flex.a = as_grouped_data(inad.tab, groups = c("sub"))
  inad.tab.flex <- flextable(inad.tab.flex.a) %>%
    theme_booktabs() %>%
    bold(bold = TRUE, part = "header") %>%
    set_header_labels(type = "",
                      sub = "") %>%
    add_footer_row(
      top = F,
      "*Pending included\n**Stool adequacy defined as per Certification Indicator, i.e., 2 stools collected at least 24h apart AND ≤14d of onset AND received in good condition at a WHO-accredited laboratory (missing condition assumed good)",
      colwidths = ncol(inad.tab)
    ) %>%
    autofit()

  return(inad.tab.flex)
}

generate_60_day_tab <- function(cases.need60day) {
  comp.by.year <- cases.need60day |>
    group_by(year) |>
    summarise(
      inadequate = n(),
      got60day = sum(got60day == 1, na.rm = T),
      ontime60day = sum(ontime.60day == 1, na.rm = T),
      compatible = sum(cdc.classification.all2 == "COMPATIBLE"),
      pot.compatible = sum(pot.compatible == 1, na.rm = T),
      missing.fu.date = sum(missing.fu.date == 1, na.rm = T)
    ) |>
    mutate(
      per.got60 = round(got60day / inadequate * 100),
      per.ontime60day = round(ontime60day / inadequate * 100),
      per.comp = round(compatible / inadequate * 100),
      per.pot.comp = round(pot.compatible / inadequate * 100),
      per.got60.2 = paste(got60day, " ", "(", per.got60, "%", ")", sep = ""),
      per.ontime60day.2 = paste(ontime60day, " ", "(", per.ontime60day, "%", ")", sep = ""),
      per.comp.2 = paste(compatible, " ", "(", per.comp, "%", ")", sep = ""),
      per.pot.comp.2 = paste(pot.compatible, " ", "(", per.pot.comp, "%", ")", sep = ""),
      per.missing.fu.date = paste(
        round(missing.fu.date / inadequate * 100),
        " ",
        "(",
        missing.fu.date,
        "%",
        ")",
        sep = ""
      )
    ) |>
    select(
      year,
      inadequate,
      per.got60.2,
      per.ontime60day.2,
      per.comp.2,
      per.pot.comp.2,
      per.missing.fu.date
    ) |>
    mutate(year = as.character(year))


  # flex table

  tab.60d <- comp.by.year |>
    flextable() |>
    theme_booktabs() |>
    bold(bold = TRUE, part = "header") |>
    set_header_labels(
      year = "Year",
      inadequate = "No. inadequate cases",
      per.got60.2 = "Recorded 60-day follow-up",
      per.ontime60day.2 = "Recorded 60-day ontime",
      per.comp.2 = "Compatible cases",
      per.pot.comp.2 = "Potentially compatible cases",
      per.missing.fu.date = "No. Missing follow up date with findings"
    ) |>
    align(j = 2:7, align = "center", part = "all") |>
    align(j = 1:1, align = "left", part = "all") |>
    fontsize(size = 11, part = "all") |>
    width(j = 1:7, width = 2) #|>

  return(tab.60d)
}

generate_timeliness_maps <- function(ctry.data, ctry.shape, prov.shape, start_date, end_date) {
  long.timely <- ctry.data$afp.all.2 %>%
    select(epid,
           noti.7d.on,
           inv.2d.noti,
           coll.3d.inv,
           ship.3d.coll,
           year,
           prov,
           adm1guid) %>%
    pivot_longer(!c(epid, year, prov, adm1guid),
                 names_to = "type",
                 values_to = "value") %>%
    group_by(year, type, prov, adm1guid) %>%
    summarize(prop = sum(value, na.rm = T) / n()) %>%
    ungroup() %>%
    filter(year >= year(start_date) & year <= year(end_date)) %>%
    complete(year, prov, type)


  for (i in 1:nrow(long.timely)) {
    if (is.na(long.timely$adm1guid[i])) {
      long.timely$adm1guid[i] = long.timely$adm1guid[which(long.timely$prov ==
                                                             long.timely$prov[i])][1]
    }
  }


  all.case <- summarize(group_by(ctry.data$afp.all.2, prov,
                                 year, adm1guid),
                        case.num = n()) %>%
    ungroup() %>%
    filter(year >= year(start_date) & year <= year(end_date)) %>%
    complete(year, prov, fill = list(case.num = 0))

  for (i in 1:nrow(all.case)) {
    if (is.na(all.case$adm1guid[i])) {
      all.case$adm1guid[i] = all.case$adm1guid[which(all.case$prov == all.case$prov[i])][1]
    }
  }

  long.timely$prop <- cut(
    long.timely$prop,
    breaks = c(-1, 0.2, 0.5, 0.8, 0.9, 1.1, 1.2),
    right = F,
    labels = c("<20%", "20-49%", "50-79%",
               "80-89%", "90-100%", NA)
  )
  long.timely$prop = fct_na_value_to_level(long.timely$prop, "Missing")# missing date data

  time.map <-
    left_join(prov.shape, long.timely, by = c("GUID" = "adm1guid"))
  time.map <-
    full_join(time.map, all.case, by = c("GUID" = "adm1guid", "year" = "year"))

  time.map <- time.map %>%
    mutate(prop = as.character(prop)) %>%
    mutate(prop = ifelse(case.num == 0, "No AFP cases", prop)) %>%
    mutate(prop = factor(
      prop,
      levels = c(
        "<20%",
        "20-49%",
        "50-79%",
        "80-89%",
        "90-100%",
        "No AFP cases",
        "Missing"
      )
    ))


  # Flag provinces with less than 5 AFP cases reported
  low.case.prov <- time.map %>%
    group_by(year, ADM1_NAME) %>%
    filter(case.num <= 5)

  # noti.7d.on

  mapt1 <- ggplot() +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    geom_sf(
      data = filter(time.map, type == "noti.7d.on"),
      color = "black",
      aes(fill = prop)
    ) +
    geom_sf(data = st_centroid(filter(low.case.prov, type == "noti.7d.on")),
            pch = 4,
            size = 4) +
    scale_fill_manual(name = "Proportion",
                      values = f.color.schemes("mapval"),
                      drop = T) +
    # scale_color_manual(values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
    #                  drop = F) +
    ggtitle("Proportion of cases with notification within 7 days of onset") +
    sirfunctions::f.plot.looks("epicurve") +
    #labs(caption = "Provinces marked by an X have reported 5 or less AFP cases")+
    facet_wrap( ~ year, ncol = 4) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      plot.caption = element_text(hjust = 0)
    )

  # inv.2d.noti
  mapt2 <- ggplot() +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    geom_sf(
      data = filter(time.map, type == "inv.2d.noti"),
      color = "black",
      aes(fill = prop)
    ) +
    geom_sf(data = st_centroid(filter(low.case.prov, type == "inv.2d.noti")),
            pch = 4,
            size = 4) +
    scale_fill_manual(name = "Proportion",
                      values = f.color.schemes("mapval"),
                      drop = T) +
    # scale_color_manual(values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
    #                  drop = F) +
    ggtitle("Proportion of cases with investigation within 2 days of notification") +
    sirfunctions::f.plot.looks("epicurve") +
    #labs(caption = "Provinces marked by an X have reported 5 or less AFP cases")+
    facet_wrap( ~ year, ncol = 4) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      plot.caption = element_text(hjust = 0)
    )

  # coll.3d.inv
  mapt3 <- ggplot() +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    geom_sf(
      data = filter(time.map, type == "coll.3d.inv"),
      color = "black",
      aes(fill = prop)
    ) +
    geom_sf(data = st_centroid(filter(low.case.prov, type == "coll.3d.inv")),
            pch = 4,
            size = 4) +
    scale_fill_manual(name = "Proportion",
                      values = f.color.schemes("mapval"),
                      drop = T) +
    # scale_color_manual(values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
    #                  drop = F) +
    ggtitle("Proportion of cases with collection within 3 days of investigation") +
    sirfunctions::f.plot.looks("epicurve") +
    #labs(caption = "Provinces marked by an X have reported 5 or less AFP cases")+
    facet_wrap( ~ year, ncol = 4) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      plot.caption = element_text(hjust = 0)
    )

  # ship.3d.coll
  mapt4 <- ggplot() +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    geom_sf(
      data = filter(time.map, type == "ship.3d.coll"),
      color = "black",
      aes(fill = prop)
    ) +
    geom_sf(data = st_centroid(filter(low.case.prov, type == "ship.3d.coll")),
            pch = 4,
            size = 4) +
    scale_fill_manual(name = "Proportion",
                      values = f.color.schemes("mapval"),
                      drop = T) +
    # scale_color_manual(values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
    #                  drop = F) +
    ggtitle("Proportion of stool shipped to lab within 3 days of collection") +
    sirfunctions::f.plot.looks("epicurve") +
    #labs(caption = "Provinces marked by an X have reported 5 or less AFP cases")+
    facet_wrap( ~ year, ncol = 4) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      plot.caption = element_text(hjust = 0)
    )

  mapt_all <-
    ggarrange(
      mapt1,
      mapt2,
      mapt3,
      mapt4,
      ncol = 2,
      nrow = 2,
      common.legend = TRUE,
      legend = "bottom"
    )

  mapt_all = annotate_figure(mapt_all,
                             bottom = text_grob("Provinces marked by an X have reported 5 or less AFP cases",
                                                hjust = 1))
  return(mapt_all)
}

generate_es_site_det <- function(ctry.data, es.data.long, es_start_date, es_end_date) {
  sias = ctry.data$sia %>%
    filter(status == "Done") %>%
    filter(yr.sia >= year(es_start_date) &
             yr.sia <= year(es_end_date)) %>%
    filter(province %in% es.data.long$ADM1_NAME)

  sias$activity.start.date = as.Date(sias$activity.start.date)
  sias$activity.end.date = as.Date(sias$activity.end.date)

  count(sias,
        yr.sia,
        province,
        activity.start.date,
        activity.end.date,
        vaccine.type)
  minsy = count(sias,
                yr.sia,
                province,
                activity.start.date,
                activity.end.date,
                vaccine.type) #%>%

  colnames(minsy)[colnames(minsy) == "province"] <- "ADM1_NAME"

  ## 22.1 ES sites & detection (es.site.det)

  new.site <- es.data.long %>%
    filter(early.dat >= min(collect.date) &
             early.dat <= max(collect.date)) %>%
    distinct(site.name, early.dat)


  minny = min(es.data.long$collect.date) - 7
  maxy = max(es.data.long$collect.date) + 7

  es.site.det <- ggplot() +
    geom_point(
      data = es.data.long |>
        arrange(ADM1_NAME),
      aes(x = collect.date, y = site.name, col = all_dets),
      pch = 19,
      size = 3
    ) +
    geom_rect(
      data = minsy,
      aes(
        xmin = activity.start.date,
        xmax = activity.end.date,
        ymin = 0,
        ymax = Inf,
        fill = vaccine.type
      ),
      alpha = 0.5
    ) +
    geom_point(
      data = es.data.long |>
        arrange(ADM1_NAME),
      aes(x = collect.date, y = site.name, col = all_dets),
      pch = 19,
      size = 3
    ) +
    geom_point(
      data = es.data.long |>
        arrange(ADM1_NAME),
      aes(x = collect.date, y = site.name),
      fill = NA,
      pch = 21,
      size = 3
    ) +
    xlab(label = "") +
    ylab(label = "Detection Sites") +
    scale_x_date(limits = c(es_start_date, es_end_date)) +
    scale_fill_manual(name = "SIAs",
                      values = c(
                        "nOPV2" = "blue",
                        "bOPV" = "coral1",
                        "mOPV2" = "purple"))+
    scale_color_manual(
      name = "ES detections",
      values = c(
        "No EV isolated" = "#f2f2f2",
        "NPEV only" = "darkgrey",
        "VDPV2" = "darkred",
        "Sabin 1" = brewer_pal(palette = "Set1")(9)[1],
        "Sabin 2" = brewer_pal(palette = "Set1")(9)[8],
        "Sabin 1/Sabin 3" = brewer_pal(palette = "Set1")(9)[2],
        "Sabin 3" = brewer_pal(palette = "Set1")(9)[3],
        "Sabin 1/Sabin 3/VDPV2" = brewer_pal(palette = "Set1")(9)[4],
        "Sabin 1/VDPV2" = brewer_pal(palette = "Set1")(9)[5],
        "Sabin 3/VDPV2" = brewer_pal(palette = "Set1")(9)[6],
        "Sabin 1 or Sabin 3" = brewer_pal(palette = "Set1")(9)[6],
        "Sabin 1/3" = brewer_pal(palette = "Set1")(9)[2],
        "Sabin 1/3 and VDPV2"  = brewer_pal(palette = "Set1")(9)[5]
      )) +
    facet_grid(ADM1_NAME ~ . ,
               scales = "free_y" ,
               space = "free",
               switch = "y") +
    theme_bw()

  return(es.site.det)
}

generate_es_det_map <- function(es.data, ctry.shape, prov.shape, es_start_date, es_end_date) {
  det.rate <- summarize(
    group_by(es.data.long, site.name),
    det.rate = 100 * sum(as.numeric(ev.detect), na.rm = TRUE) / n(),
    samp.num = n()
  )

  det.rate$cats <- cut(
    det.rate$det.rate,
    breaks = c(0, 50, 80, 101),
    right = F,
    labels = c("<50%", "50-79%", "80-100%")
  )

  det.rate$cats <- as.character(det.rate$cats)

  det.rate <- det.rate %>%
    mutate(cats = case_when(samp.num < 5 ~ "<5 samples",
                            TRUE ~ cats))

  site.coord <-
    reframe(group_by(es.data, site.name), lat = lat, lng = lng)
  site.coord <- unique(site.coord)

  det.rate$cats <- factor(det.rate$cats,
                          levels = c("<50%", "50-79%", "80-100%", "<5 samples"))

  es.data <-
    left_join(es.data, det.rate, by = c("site.name" = "site.name"))

  det.rate <- left_join(det.rate, site.coord)


  # ES Map of sites
  #randomly put points in their districts

  es.det.map <- ggplot() +
    geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    geom_sf(
      data = prov.shape,
      color = "black",
      fill = NA,
      size = .5
    ) +
    geom_point(data = det.rate,
               aes(
                 x = as.numeric(lng),
                 y = as.numeric(lat),
                 color = cats
               )) +
    geom_label_repel(
      data = subset(det.rate, site.name != "OSHIKANGO TREATMENT PLANT"),
      aes(
        x = as.numeric(lng),
        y = as.numeric(lat),
        label = site.name,
        color = cats,
      ),
      show.legend = FALSE,
      force = 100
    ) +
    ggtitle(paste0(
      "ES detection rate by site: ",
      format(es_start_date, "%B %Y"),
      " - ",
      format(es_end_date, "%B %Y")
    )) +
    scale_color_manual(
      values = c(
        "<50%" = "#FF0000",
        "50-79%" = "#feb24c",
        "80-100%" = "#0070c0",
        "<5 samples" = "black"
      ),
      name = "EV detection rate",
      drop = F
    ) +
    sirfunctions::f.plot.looks("02") +
    theme(legend.position = "right")

  return(es.det.map)
}

generate_es_timely <- function(es.data, es_start_date, es_end_date) {
  es.data$timely <-
    difftime(
      as.Date(es.data$date.received.in.lab, format = "%d/%m/%Y"),
      es.data$collect.date,
      unit = "days"
    )

  per.time = es.data %>% count(timely > 3) %>%
    rename(c("timely" = `timely > 3`, "n" = "n"))
  # The number that are false are the percentage timely

  per.timely.title = paste0(
    round(100 * filter(per.time, timely == FALSE)["n"] / sum(per.time$n), 0),
    "% of samples were shipped to lab within 3 days of collection - \n",
    format(es_start_date, "%B %Y"),
    " - ",
    format(es_end_date, "%B %Y")
  )

  miss.samp = filter(per.time, is.na(timely))

  num.miss.capt = paste0(
    ifelse(dim(miss.samp)[1] == 0, 0,
           (miss.samp["n"])),
    " (",
    round(100 * as.numeric(ifelse(
      dim(miss.samp)[1] == 0, 0,
      (miss.samp["n"])
    )) / sum(per.time$n), 0),
    "%) samples were missing date information"
  )

  # Timeliness of ES
  # Excludes those with bad data (e.g. negative timeliness)
  es.timely <- ggplot() +
    geom_hline(
      yintercept = 3,
      color = "dark gray",
      linetype = "dashed",
      lwd = 1
    ) +
    geom_point(
      data = filter(es.data,
                    timely >= 0),
      aes(x = collect.date, y = timely, color = site.name),
      alpha = 0.7,
      position = position_jitter(height = .2, width = 0.5),
      size = 3
    ) +
    scale_y_continuous(labels = number_format(accuracy = 1),
                       breaks = c(seq(0, max(
                         pretty(es.data$timely)
                       ), 6))) +
    labs(x = "Date of collection", y = "Transport time to lab (days)",
         color = "Site Name") +
    labs(title = per.timely.title,
         caption = num.miss.capt) +
    # scale_x_date(date_breaks = "2 months", date_labels = "%b-%y", limits = c(start.date.12m, end.date + months(1))) +
    theme_classic() +
    theme(
      text = element_text(size = 16),
      axis.text = element_text(size = 14),
      plot.caption = element_text(hjust = 0)
    )

  return(es.timely)
}

generate_es_table <- function(es.data, es_start_date, es_end_date) {
  # Big table that needs calculating
  # Cols = province, district, site name, earliest sample collected in POLIS,
  # n samples collected (earliest to analysis date), % EV detected, % good condition
  # % arrived within 3 days, days from collection to lab arrival (median + range),
  # WPV/VDPV

  # ev.pct = ev percent
  # condition percent
  # transport percent
  # median transport days
  # DATES MAY NEED ADJUSTMENT DEPENDING ON ANALYSIS PERIOD


  # NOTE THAT RIGHT NOW THIS REMOVES SAMPLES MISSING A CONDITION FROM THE CALCULATIONS
  es.data$timely <-
    difftime(
      as.Date(es.data$date.received.in.lab, format = "%d/%m/%Y"),
      es.data$collect.date,
      unit = "days"
    )

  es.tab1 <- es.data %>%
    #filter(year(collect.date) == year(end_date)) %>%
    group_by(site.name, ADM1_NAME, ADM2_NAME) %>%
    reframe(
      early.dat = format(early.dat, format = "%B %d, %Y"),
      # earliest report to POLIS
      ev.pct = 100 * sum(as.numeric(ev.detect), na.rm = TRUE) / n(),
      # percent ev detected
      num.spec = n(),
      # number of specimens
      condition.pct = 100 * sum(sample.condition == "Good", na.rm = T) / n(),
      # specimens in good condition
      trans.pct = 100 * sum(as.numeric(timely) <= 3, na.rm = TRUE) / n(),
      # % timely
      med.trans = paste0(
        median(as.numeric(timely), na.rm = T),
        " (",
        min(as.numeric(timely), na.rm = T),
        ", ",
        max(as.numeric(timely), na.rm = T),
        ")"
      ),
      # med (range)
      num.wpv.or.vdpv = sum(wpv, na.rm = T) + sum(vdpv, na.rm = T)
    ) %>% # WPV/VDPV
    distinct()

  es.tab1 = es.tab1 %>%
    arrange(ADM1_NAME, ADM2_NAME, site.name)

  es.table <- es.tab1 %>%
    flextable(
      col_keys = c(
        "ADM1_NAME",
        "ADM2_NAME",
        "site.name",
        "early.dat",
        "num.spec",
        "ev.pct",
        "condition.pct",
        "trans.pct",
        "med.trans",
        "num.wpv.or.vdpv"
      )
    ) %>%
    theme_booktabs() %>%
    add_header_lines(values = paste0(
      format(es_start_date, "%B %Y"),
      " - ",
      format(es_end_date, "%B %Y")
    )) %>%
    # hline(part="all", border = gray.border ) %>%
    bold(bold = TRUE, part = "header") %>%
    # hline(part = "header", border = std.border) %>%
    align(j = 4:9, align = "center", part = "all") %>%
    align(j = 1:3, align = "left", part = "all") %>%
    # hline(part = "header", border = std.border) %>%
    # hline_bottom(part = "body", border = std.border ) %>%
    colformat_double(j = 5:8,
                     digits = 0,
                     na_str = "NA") %>%
    width(width = 1) %>%
    width(j = 3, width = 2.5) %>%
    width(j = 1:2, width = 1.5) %>%
    # width(j=10, width = .1) %>%
    #add_footer(province = "Red = indicator not met, * and gray = missing data for >25% of samples, NA = data unavailable; Indicator targets: >=50% for EV (NPEV, vaccine, VDPV, or WPV) detection, =>80% for sample condition and transport time. Sites with <6 months of sample collection are labeled as 'new'.") %>%
    #merge_at(j = 1:9, part = "footer") %>%
    fontsize(size = 11, part = "all") %>%
    set_header_labels(
      ADM1_NAME = "Province",
      ADM2_NAME = "District",
      early.dat = "Earliest date reporting to POLIS",
      site.name = "Site name",
      num.spec = "No. samples collected",
      ev.pct = "% detected EV",
      condition.pct = "% good condition",
      trans.pct = "% arriving within 3 days",
      med.trans = "Median lab transport time (d)",
      num.wpv.or.vdpv = "No. VDPV or WPV"
    )

  return(es.table)
}

generate_case_num_dose_g <- function(ctry.data, start_date, end_date) {
  dose.num.cols = c(
    "0" = "#C00000",
    "1-2" = "#FFC000",
    "3" = "#92D050",
    "4+" = "#548235",
    "Missing" = "#A5A5A5"
  )

  ### Create zero dose graphs ----
  # Cats - 0, 1-2, 3, 4+
  dcat.yr.prov = summarize(group_by(
    ctry.data$afp.all.2 |>
      filter(date >= start_date &
               date <= end_date,
             cdc.classification.all2 == "NPAFP",
             between(age.months, 6, 59)),
    dose.cat,
    year,
    prov
  ),
  freq = n())

  # case num by year and province by vaccination status
  case.num.dose.g = ggplot() +
    geom_bar(
      data = dcat.yr.prov,
      aes(x = year, y = freq, fill = dose.cat),
      stat = "identity",
      position = "fill"
    ) +
    xlab("") +
    ylab("Percent of Cases") +
    scale_fill_manual("Number of doses - IPV/OPV", values = dose.num.cols, drop = F) +
    scale_y_continuous(labels = scales::percent)  +
    labs(title="OPV/IPV Status of NP AFP cases, 6-59 months") +
    theme_pubr()

  return(case.num.dose.g)
}
