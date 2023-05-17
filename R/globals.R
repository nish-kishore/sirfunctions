
utils::globalVariables(c(
  'ADM0_NAME', 'adequacy.final', 'adm0guid', 'adm1guid', 'adm2guid', 'afp.cases',
  'afpes.detection.35d', 'afpes.detection.49d', 'afpes.pos.spec', 'cdc.classification.all',
  'collect.date', 'collect.to.enddate', 'collect.yr', 'ctry', 'date.f6.ref.itd',
  'date.final.combined.result', 'date.final.culture.result', 'date.final.results.reported
  date.notification.to.hq', 'datenotificationtohq', 'datenotifytohq', 'days.at.risk',
  'days_in_year', 'dist', 'earliest_date', 'env.sample.id', 'epid', 'ev.detect',
  'ev.rate', 'freq', 'latest_date', 'median', 'median_days', 'missing.data', 'n_days',
  'n_npafp', 'num.ad', 'num.adequate', 'num.adj.w.miss', 'num.afpes.detect.35d',
  'num.afpes.detect.49d', 'num.ctry.inseq', 'num.ctry.outseq', 'num.data.error',
  'num.ev.pos', 'num.inadequate', 'num.missing.data', 'num.samples', 'ontonothq',
  'par', 'pct.afpes.detect.35d', 'pct.afpes.detect.49d', 'per.stool.ad', 'percent',
  'pivot_longer', 'prov', 'pv', 'replace_na', 'seq.capacity', 'site.name', 'stoolad',
  'toty', 'u15pop', 'value', 'vdpv', 'weight', 'who.region', 'wpv', '.', 'date.notification.to.hq',
  'date.final.results.reported'
))

# "ADM0_NAME adequacy.final adm0guid adm1guid adm2guid afp.cases
#     afpes.detection.35d afpes.detection.49d afpes.pos.spec
#     cdc.classification.all2 collect.date collect.to.enddate collect.yr
#     ctry date.f6.ref.itd date.final.combined.result
#     date.final.culture.result date.final.results.reported
#     date.notification.to.hq datenotificationtohq datenotifytohq
#     days.at.risk days_in_year dist earliest_date env.sample.id epid
#     ev.detect ev.rate freq latest_date median median_days missing.data
#     n_days n_npafp num.ad num.adequate num.adj.w.miss
#     num.afpes.detect.35d num.afpes.detect.49d num.ctry.inseq
#     num.ctry.outseq num.data.error num.ev.pos num.inadequate
#     num.missing.data num.samples ontonothq par pct.afpes.detect.35d
#     pct.afpes.detect.49d per.stool.ad percent pivot_longer prov pv
#     replace_na seq.capacity site.name stoolad toty u15pop value vdpv
#     weight who.region wpv" |>
#   stringr::str_replace_all("\n", "") |>
#   stringr::str_split(" ") -> x
#
# paste0("'",x[[1]][(sapply(x, nchar, simplify = F)[[1]] > 0)] |>
#          paste0(collapse = "', '"),"'")

