reframe_wea_sql <- function(usg_typ, terr_cd, usg_yr, pred_yr, wea_stn_owner, tou = "hetoua")  {
  setwd("~")
  setwd('Documents/dstrain/')
  if (usg_typ == "train") {
    wea_data_typ_id <- "(1)"
  } else {
    wea_data_typ_id <- "(22, 23)"
  }
  step_num <- "06"
  out_ext <- ".sql"

  wea_stn_owner_quo <- paste0("'", wea_stn_owner, "'")

  obj_in <- stringr::str_to_lower(paste0("`",paste("wea",usg_yr, tou, sep = "_"),"`"))
  obj_out <- stringr::str_to_lower(paste("wea",usg_yr, tou, sep = "_"))
  # pop_in <- stringr::str_to_lower(paste0("`",paste("model_population", terr_cd, sep = "_"),"`"))

  file_nm <- stringr::str_to_lower(paste0(step_num,"_", obj_out, out_ext))

  out_path <- paste0('data/sql/',file_nm)

create_head <- paste(
  paste0("DROP TABLE IF EXISTS `rda`.`rdadata`.",obj_in),
  paste0("CREATE TABLE `rda`.`rdadata`.",obj_in), sep = ';\n')

part_body <- c(
  "(calendar_date,
  month_of_year,
  day_of_month,
  train_year,
  predict_year,
  day_of_week,
  week_of_year,
  quarter_of_year,
  day_of_year,
  day_of_year_shift,
  wea_data_typ_id,
  wea_data_typ_cd,
  baseline_terr_cd,
  opr_area_cd,
  wea_hr,
  rt_sched_cd,
  tou_cd,
  meas_val)"
  )

part_str <-  c(
  "PARITION BY (day_of_year)")

agg_head <-  c(
  "AS (SELECT wea.calendar_date,
  wea.month_of_year,
  wea.day_of_month,
  wea.train_year,
  wea.predict_year,
  wea.day_of_week,
  wea.week_of_year,
  wea.quarter_of_year,
  wea.day_of_year,
  wea.day_of_year_shift,
  wea.wea_data_typ_id,
  wea.wea_data_typ_cd,
  wea.baseline_terr_cd,
  wea.opr_area_cd,
  wea.wea_hr,
  tou.rt_sched_cd,
  tou.tou_cd,
  AVG(wea.meas_val) AS meas_val FROM"
)

join_head <- c(
  "(SELECT xref.calendar_date,
  xref.month_of_year,
  xref.day_of_month,
  xref.year_of_calendar,
  xref.year_of_calendar_shift,
  xref.predict_year,
  xref.day_of_week,
  xref.week_of_year,
  xref.quarter_of_year,
  xref.day_of_year,
  xref.day_of_year_shift,
  wea.wea_data_typ_id,
  wea.wea_dt,
  wea.wea_dttm,
  typ.wea_data_typ_cd,
  terr_xref.baseline_terr_cd,
  area_xref.opr_area_cd,
  DATE_PART('hour', wea.wea_dttm) AS wea_hr,
  CAST(wea.meas_val AS FLOAT) AS meas_val"
  )

from_body <- c(
  "FROM `rda`.`rdadata`.`time_shift_xref` AS xref"
  )

join_body <- c(
"JOIN `rda`.`rdatables`.`weather_data` AS wea
  ON xref.calendar_date = wea.wea_dt
  JOIN `rda`.`rdatables`.`weather_station` AS stn
  ON wea.wea_stn_cd = stn.wea_stn_cd
  JOIN `rda`.`rdatables`.`weather_data_type` AS typ
  ON wea.wea_data_typ_id = typ.wea_data_typ_id
  INNER JOIN `rda`.`rdatables`.`weather_baseline_terr_xref` AS terr_xref
  ON wea.wea_stn_cd = terr_xref.wea_stn_cd
  AND xref.month_of_year = terr_xref.mo_id
  JOIN `rda`.`rdatables`.`weather_op_area_xref` AS area_xref
  ON terr_xref.wea_stn_cd = area_xref.wea_stn_cd"
)

where <- c(
  paste("WHERE xref.year_of_calendar =", usg_yr,
        "AND xref.year_of_calendar_shift =", pred_yr,
        "AND wea.wea_data_typ_id IN", wea_data_typ_id,
        "AND stn.wea_stn_owner =", wea_stn_owner_quo,") AS wea")
  )

tou_body <- c(
"JOIN `rda`.`rdadata`.`tou_lookup_hetoua` AS tou
ON wea.year_of_calendar = tou.year_of_calendar
ON wea.year_of_calendar_shift = tou.year_of_calendar_shift
ON wea.month_of_year, = month_of_year,
ON wea.day_of_year, = tou.day_of_year,
AND wea.wea_dttm BETWEEN tou.tou_data_from_dttm AND tou.tou_data_to_dttm")

agg_body <- c(
  "GROUP BY
  wea.calendar_date,
  wea.month_of_year,
  wea.day_of_month,
  wea.train_year,
  wea.predict_year,
  wea.day_of_week,
  wea.week_of_year,
  wea.quarter_of_year,
  wea.day_of_year,
  wea.day_of_year_shift,
  wea.wea_data_typ_id,
  wea.wea_data_typ_cd,
  wea.baseline_terr_cd,
  wea.opr_area_cd,
  wea.wea_hr,
  tou.rt_sched_cd,
  tou.tou_cd)")

wea_tou_query <- list(create_head,part_body, part_str,agg_head, join_head, from_body, join_body, where, tou_body, agg_body)
readr::write_lines(wea_tou_query, path  = out_path, append = FALSE)

}

terr_cd <- 'X'
usg_yr <-  2017
pred_yr <-  2018
tou <-  "hetoua"
usg_typ <-  "train"
wea_stn_owner <-  "PGE"

reframe_wea_sql(usg_typ = "train", terr_cd = 'X', usg_yr = 2017, pred_yr =  2018
                , tou = "hetoua", wea_stn_owner =  "PGE")
