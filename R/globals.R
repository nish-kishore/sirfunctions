
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
  'date.final.results.reported', 'raw.data', 'ADM0_GUID', 'ADM0_SOVRN', 'ADM1_GUID',
  'ADM1_NAME', 'ADM2_NAME', 'ENDDATE', 'GUID', 'STARTDATE', 'adm0_name', 'adm1_name',
  'adm2_name', 'cdc.classification.all2', 'dateinvest', 'datenotify', 'dateonset', 'dpt',
  'dpt1', 'dpt3', 'epi.week', 'f.yrs.01', 'final.cell.culture.result', 'hot.case',
  'iris', 'lat', 'lon', 'mcv1', 'paralysis.asymmetric', 'paralysis.onset.fever', 'paralysis.rapid.progress',
  'person.sex', 'place.admin.0', 'place.admin.1','place.admin.2', 'surveillancetypename',
  'under5_pop','yr.end','yronset', 'yr.st', 'name', 'lastModified', 'size', 'last_sync', 'ctime', 'last_user',
  'sub.activity.start.date', 'sia.code', 'vaccine.type', 'resource', 'token', 'end.yr', 'start.yr', 'file_name',
  'scope', 'tag', 'year.var', 'end_date', 'start_date',
  # new additions to v1.1
  'CITY_NAME', 'CNTRY_NAME', 'CaseOrContact', 'DateFinalCellCultureResult', 'DateOfOnset', 'DateStoolCollected',
  'EPID', 'EpidNumber', 'Name', 'ParalysisOnsetDate', 'SpecimenNumber', '_gps_ending_latitude', '_gps_ending_longitude',
  'activity.end.date', 'activity.start.date', 'add_slide', 'adequacy.final2', 'adm2guid.2', 'admin2guid', 'age.months',
  'all_dets', 'block_list', 'case.cat', 'case.num', 'case.num.dose.g', 'cases', 'cats', 'cdc.class', 'classification',
  'clust', 'coll.3d.inv', 'conds', 'corrupted.rds', 'country', 'ctry.code2', 'ctryvdist_diff', 'ctryvprov_diff',
  'date.invest', 'date.notify', 'date.onset', 'datestool1', 'datestool2', 'days.collect.lab', 'days.itd.arriveseq',
  'days.itd.seqres', 'days.lab.culture', 'days.lab.seq', 'days.seq.rec.res', 'days.seq.ship', 'daysstooltolab',
  'diff_per', 'dist.guid', 'dml', 'dose.cat', 'doses.ipv.number', 'doses.ipv.routine', 'doses.ipv.sia', 'doses.opv.routine',
  'doses.opv.sia', 'doses.total', 'e', 'early.dat', 'emergencegroup', 'empty.01', 'epid_ctry', 'epid_dist', 'epid_prov',
  'external_img', 'fct_inorder', 'followup.date', 'followup.findings', 'fp_text', 'fpar', 'ftext', 'get(geo)', 'got60day',
  'hasArg', 'hf_rating', 'inv.2d.noti', 'investtostool1', 'layout_properties', 'layout_summary', 'lng', 'medi', 'meet.stool',
  'meet2', 'missing.fu.date', 'mon.year', 'mon.year2', 'month_date', 'n_det', 'n_det_cat', 'need60day.v2', 'noti.7d.on',
  'nottoinvest', 'npafp_rate', 'ontime.60day', 'ontonot', 'ph_location', 'ph_location_label', 'ph_location_type', 'ph_with',
  'pop.tab', 'pot.compatible', 'priority_level', 'prop', 'prop.cat', 'prop.dist.adeq', 'province', 'read_pptx', 'ship.3d.coll',
  'status', 'stool.1.condition', 'stool.2.condition', 'stool.col.int', 'stool.date.sent.to.lab', 'stool1missing', 'stool1tostool2',
  'stool2missing', 'stoolmissing', 't.daysstooltolab', 't.investtostool1', 't.nottoinvest', 't.ontonot', 't.stool1tostool2', 'timely',
  'timely > 3', 'timelystool', 'today_date', 'tot.dist.adeq', 'tot.freq.x', 'tot.freq.y', 'type', 'u15pop.prov', 'vdpv.1', 'vdpv.2',
  'vdpv.3', 'wild.1', 'wild.3', 'yr.sia'


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

