#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @import ggplot2
#' @import lubridate
#' @import rlang
#' @importFrom AzureAuth get_azure_token
#' @importFrom AzureAuth list_azure_tokens
#' @importFrom AzureAuth load_azure_token
#' @importFrom AzureStor create_storage_dir
#' @importFrom AzureStor delete_storage_file
#' @importFrom AzureStor list_storage_files
#' @importFrom AzureStor storage_container
#' @importFrom AzureStor storage_dir_exists
#' @importFrom AzureStor storage_download
#' @importFrom AzureStor storage_endpoint
#' @importFrom AzureStor storage_file_exists
#' @importFrom AzureStor storage_load_rdata
#' @importFrom AzureStor storage_load_rds
#' @importFrom AzureStor storage_read_csv
#' @importFrom AzureStor storage_save_rds
#' @importFrom AzureStor storage_upload
#' @importFrom AzureStor storage_write_csv
#' @importFrom cli cli_abort
#' @importFrom cli cli_alert
#' @importFrom cli cli_alert_danger
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_h1
#' @importFrom cli cli_process_done
#' @importFrom cli cli_process_start
#' @importFrom cli cli_progress_done
#' @importFrom cli cli_progress_step
#' @importFrom glue glue
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @importFrom lifecycle badge
#' @importFrom lifecycle deprecate_warn
#' @importFrom lifecycle deprecated
#' @importFrom lifecycle is_present
#' @importFrom purrr map
#' @importFrom rappdirs user_data_dir
#' @importFrom readr read_csv
#' @importFrom readr read_rds
#' @importFrom readr write_csv
#' @importFrom readr write_lines
#' @importFrom readr write_rds
#' @importFrom scales brewer_pal
#' @importFrom scales comma
#' @importFrom scales label_percent
#' @importFrom scales number_format
#' @importFrom scales percent
#' @importFrom sf sf_use_s2
#' @importFrom sf st_as_sf
#' @importFrom sf st_as_sfc
#' @importFrom sf st_bbox
#' @importFrom sf st_centroid
#' @importFrom sf st_coordinates
#' @importFrom sf st_crop
#' @importFrom sf st_crs
#' @importFrom sf st_intersection
#' @importFrom sf st_is_empty
#' @importFrom sf st_make_valid
#' @importFrom sf st_sample
#' @importFrom sf st_touches
#' @importFrom sf st_transform
#' @importFrom stats setNames
#' @importFrom stringr regex
#' @importFrom stringr str_c
#' @importFrom stringr str_count
#' @importFrom stringr str_detect
#' @importFrom stringr str_ends
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringr str_squish
#' @importFrom stringr str_starts
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_to_title
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_trim
#' @importFrom tidyr complete
#' @importFrom tidyr drop_na
#' @importFrom tidyr expand_grid
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#' @importFrom tidyr separate_wider_regex
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @importFrom utils download.file
#' @importFrom utils globalVariables
#' @importFrom utils head
#' @importFrom withr with_tempdir
#' @importFrom yaml read_yaml
## usethis namespace: end
NULL

utils::globalVariables(c(
  ".", "_gps_ending_latitude", "_gps_ending_longitude", "active.year.01",
  "activity.end.date", "activity.start.date", "ad.dist.100k.num", "adequacy.01",
  "adequacy.02", "adequacy.03", "adequacy.final", "adequacy.final2",
  "ADM0_GUID", "adm0_name", "ADM0_NAME", "ADM0_SOVRN", "adm0guid", "adm0guid.x",
  "adm0guid.y", "ADM1_GUID", "adm1_name", "ADM1_NAME", "adm1guid", "adm1guid.x",
  "adm1guid.y", "adm2_name", "ADM2_NAME", "adm2guid", "adm2guid.2",
  "adm2guid.x", "adm2guid.y", "admin2guid", "afp.cases", "afpes.detection.35d",
  "afpes.detection.49d", "afpes.pos.spec", "age.group", "age.months",
  "all_dets", "bad.condition", "bad.data", "badper", "case.cat", "case.num",
  "CaseOrContact", "cats", "cdc.class", "cdc.classification.all",
  "cdc.classification.all2", "CITY_NAME", "classification", "clust",
  "CNTRY_NAME", "coll.3d.inv", "collect.date", "collect.to.enddate",
  "collect.yr", "collection.date", "column_name", "compatible", "country",
  "ctime", "ctry", "ctry.x", "ctry.y", "ctryvdist_diff", "ctryvprov_diff",
  "culture.itd.lab", "datasource", "date.f6.ref.itd",
  "date.final.combined.result", "date.final.culture.result",
  "date.final.results.reported", "date.invest", "date.notification.to.hq",
  "date.notify", "date.onset", "DateFinalCellCultureResult",
  "DateFinalCellCultureResults", "DateFinalrRTPCRResults", "dateinvest",
  "DateIsolateRcvdForSeq", "datenotificationtohq", "datenotify",
  "datenotifytohq", "DateOfOnset", "DateofSequencing", "dateonset",
  "DateSeqResult", "DateSeqResultsEntered", "datestool1", "datestool2",
  "DateStoolCollected", "DateStoolReceivedinLab", "DateStoolReceivedNatLevel",
  "DateStoolSentfromField", "DateStoolSentToLab", "days.collect.lab",
  "days.itd.arriveseq", "days.itd.seqres", "days.lab.culture", "days.lab.seq",
  "days.seq.rec.res", "days.seq.ship", "days_in_year", "daysstooltolab",
  "detect_status", "detect_status2", "diff_per", "dist", "dist.100k.num",
  "dist.guid", "dist.x", "dist.y", "dists", "dose.cat", "doses.ipv.number",
  "doses.ipv.routine", "doses.ipv.sia", "doses.opv.routine", "doses.opv.sia",
  "doses.total", "dpt1", "dpt3", "due.60followup", "e", "earliest_date",
  "early.dat", "emergence.group", "emergencegroup", "emg_grp2", "end.yr",
  "end_date", "ENDDATE", "env.sample.id", "epi.week", "epid", "EPID",
  "epid.in.polis", "epid_comb", "epid_ctry", "epid_dist", "epid_prov",
  "EpidNumber", "es.data.test", "es.dups", "ev.detect", "ev.rate",
  "facility_name2", "file_name", "FinalCellCultureResult", "FinalITDResult",
  "followup.date", "followup.findings", "freq", "geometry", "good.cond.1",
  "good.cond.1per", "good.cond.2", "good.cond.2per", "got60day", "GUID",
  "hasArg", "hf_rating", "hot.case", "im.loaded", "imputed_coord", "inadequate",
  "inadequate.need.60day", "inv.2d.noti", "investtostool1", "iris", "lab_data",
  "last_sync", "last_user", "lat", "lat.x", "lat.y", "late.collection",
  "latest_date", "latitude", "len.year", "lng", "lng.x", "lng.y", "lon",
  "longitude", "lqas.loaded", "mcv1", "measurement", "medi", "median",
  "median_days", "meet.stool", "meet2", "missing.fu.date", "missingper",
  "mon.year", "mon.year2", "month_date", "n_days", "n_det", "n_det_cat",
  "n_npafp", "name", "Name", "need60day.v2", "neg_intervals", "noti.7d.on",
  "nottoinvest", "npafp_rate", "nt.changes", "ntchanges", "num.ad.plus.inad",
  "num.adequate", "num.afpes.detect.35d", "num.afpes.detect.49d",
  "num.ctry.inseq", "num.ctry.outseq", "num.ev.pos", "num.inadequate",
  "num.meet.stool", "num.meet2", "num.samples", "one.or.no.stool",
  "ontime.60day", "ontime60day", "ontonot", "ontonothq", "ontostool1",
  "ontostool2", "par", "paralysis.asymmetric", "paralysis.onset.fever",
  "paralysis.rapid.progress", "ParalysisOnsetDate", "paths",
  "pct.afpes.detect.35d", "pct.afpes.detect.49d", "per.got60",
  "per.ontime60day", "per.stool.ad", "person.sex", "place.admin.0",
  "place.admin.1", "place.admin.2", "polis.case.id", "pons.epid",
  "poorper", "pot.compatible", "priority_level", "prop", "prop.cat",
  "prop.dist.adeq", "prov", "prov.x", "prov.y", "province", "pv", "raw.data",
  "report_date", "ReportDateSequenceResultSent", "resource", "sample.condition",
  "sampled_point", "samples", "scope", "seq.capacity", "SHAPE", "ship.3d.coll",
  "sia.code", "sia.sub.activity.code", "site.id", "site.name", "source_mod",
  "SpecimenNumber", "start.yr", "start_date", "STARTDATE", "status",
  "stool.1.condition", "stool.2.condition", "stool.col.int",
  "stool.date.sent.to.lab", "stool1missing", "stool1tostool2", "stool2missing",
  "sub.activity.start.date", "surveillancetypename", "t.daysstooltolab",
  "t.investtostool1", "t.nottoinvest", "t.ontonot", "t.stool1tostool2", "tag",
  "timely", "timelyper", "today_date", "token", "tot.dist.adeq", "tot.freq.x",
  "tot.freq.y", "total_rows", "type", "u15pop", "u15pop.prov", "under5_pop",
  "vaccine.type", "value", "vdpv", "vdpv.1", "vdpv.2", "vdpv.3", "virus.type",
  "virustype", "vtype_mod", "weight", "who.region", "WHO_CODE", "WHO_REGION",
  "wild.1", "wild.3", "wpv", "year2", "years_active", "yr.end", "yr.sia",
  "yr.st", "yronset","nums", "diffs", "set_iterator",

  # From calc_hr_range()
  "set",

  # From create_polis_data_folder()
  "src_path", "dest_path", "isdir", "size",

  # From get_cdc_childvaxview_data()
  "geography", "coverage_estimate",
  "population_sample_size", "vaccine", "_95_ci",

  # From get_vpd_data()
  "country_name", "vpd", "variable", "iso3_code", "vpd_short_name",

  # From sirfunctions_io()
  "isdir", "lastModified", "mtime", "uid",

  # From KPI
  "CaseDate",  "Region",  "SG",  "Priority",  "Level",  "Shape",  "VDPV1",  "VDPV2",  "VDPV3",  "WILD1",
  "active_sites",  "adequacy.denominator",  "adm0guid_afp",  "adm0guid_pop",
  "adm1guid_afp",  "adm1guid_pop",  "adm2guid_afp",  "adm2guid_pop",  "afp_cases",
  "analysis_year_end",  "analysis_year_start",  "archived_data",  "category",
  "consistent_guid",  "ctry.short",  "culture.itd.cat",  "culture_itd_lab",
  "date_interval",  "det_denom",  "dist_stool",  "dist_w_100k",  "es_samples",  "es_sites",
  "es_sites_w_age",  "ev_denom",  "ev_label",  "ev_rate",  "good_samples",  "indicator",
  "is_target",  "label",  "met_ev",  "met_ev_5_samples",  "met_ev_label",  "met_good_samples",
  "met_npafp",  "met_npafp_label",  "met_stool",  "met_stool_label",
  "met_timely_wild_vdpv",  "met_timely_wild_vdpv_label",
  "met_timely_wpv_vdpv_det",  "n_na",  "n_samples_12_mo",  "npafp_cat",  "npafp_denom",
  "npafp_label",  "ontoinvest",  "ontolab",  "ontostool_final",  "per_stool_ad",
  "prop_good_es",  "prop_met_ev",  "prop_met_ev_5_samples",
  "prop_met_ev_5_samples_label",  "prop_met_ev_label",  "prop_met_good_samples",
  "prop_met_good_samples_label",  "prop_met_npafp",  "prop_met_stool",
  "prop_met_timely_wild_vdpv",  "prop_met_timely_wpv_vdpv_det",
  "prop_met_timely_wpv_vdpv_det_label",  "prop_t1_label",  "prop_t2_label",
  "prop_t3_label",  "prop_t4_label",  "prop_t5_label",  "prop_timely_det_wpv_vdpv",
  "prop_timely_isolation",  "prop_timely_itd",  "prop_timely_itdres_seqres",
  "prop_timely_seqres",  "prop_timely_seqship",  "prop_timely_ship",
  "prop_timely_wild_vdpv",  "prop_timely_wild_vdpv_label",  "raw_data",  "region",
  "rolling_period",  "sampling_interval",  "seq.cat",  "seq.lab",  "seq_lab",  "setNames",
  "sg_priority_level", "SG Priority Level", "site_age",  "stool_collection_date",  "stool_denom",
  "stool_label",  "t1",  "t2",  "t3",  "t4",  "t5",  "timely_cat",  "timely_det",  "timely_isolation",
  "timely_itd",  "timely_itdres_seqres",  "timely_seqres",  "timely_seqship",
  "timely_ship",  "timely_wpv_vdpv_detections",  "wgs.lab*",  "who_region",  "whoregion",
  "wild_vdpv",  "wpv_vdpv_detections",  "y",  "year_label",  "year_num",  "year_number"

))
