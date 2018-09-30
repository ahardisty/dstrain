xy_tou_agg_sql <- function(usg_typ, terr_cd, usg_yr, tou = "hetoua")  {
  setwd("~")
  setwd('Documents/dstrain/')

  step_num <- "06"
  out_ext <- ".sql"
  # proj_nm <- "eua"
  # dir_out <-  'data_prep'
  # schema_in <- '`rda`.`rdatables`.'
  # schema_out <- '`rda`.`rdadata`.'
  # tbl_in <- 'model_population'
  # tbl_out <- "model_population"
  # terr_cd_quo <- paste0("'",terr_cd,"'")

  obj_in <- stringr::str_to_lower(paste0("`",paste("usg",usg_yr, tou, terr_cd, sep = "_"),"`"))
  obj_out <- stringr::str_to_lower(paste(usg_typ,usg_yr, tou, terr_cd, sep = "_"))

  pop_in <- stringr::str_to_lower(paste0("`",paste("wea",usg_yr,tou, sep = "_"),"`"))
  # pop_out <- stringr::str_to_lower(paste0("`",paste("wea",usg_yr,tou,sep = "_"),"`"))

  file_nm <- stringr::str_to_lower(paste0(step_num,"_", obj_out, out_ext))

  out_path <- paste0('data/sql/',file_nm)

  create_head <- paste(
    paste0("DROP TABLE IF EXISTS `rda`.`rdadata`.",obj_out),
    paste0("CREATE TABLE `rda`.`rdadata`.",obj_out), sep = ';\n')

  create_head <- paste(
    paste0("DROP TABLE IF EXISTS `rda`.`rdadata`.",obj_out),
    paste0("CREATE TABLE `rda`.`rdadata`.",obj_out), sep = ';\n')

  part_body <- c(
    "(group_id,
    model_id,
    uniq_sa_id,
    baseline_terr_cd,
    opr_area_cd,
    train_year,
    predict_year,
    rt_sched_cd,
    tou_cd,
    wea_data_typ_id,
    day_of_year_shift,
    X,
    Y)"
    )

  part_str <-  c(
    "PARITION BY (group_id)")

  agg_head <-  c(
    "AS (SELECT
    cust_usg.group_id,
    cust_usg.model_id,
    cust_usg.uniq_sa_id,
    cust_usg.deriv_baseline_terr_cd,
    cust_usg.opr_area_cd,
    cust_usg.train_year,
    cust_usg.predict_year,
    cust_usg.day_of_year,
    cust_usg.day_of_year_shift,
    cust_usg.rt_sched_cd,
    cust_usg.tou_cd,
    AVG(wea_tou.meas_val) AS X,
    SUM(cust_usg.usg_amt) AS Y
    FROM"
  )

  join_head <- c(
    "(SELECT
    cust_usg.group_id,
    cust_usg.model_id,
    cust_usg.uniq_sa_id,
    cust_usg.deriv_baseline_terr_cd,
    cust_usg.opr_area_cd,
    cust_usg.calendar_date,
    cust_usg.month_of_year,
    cust_usg.day_of_month,
    cust_usg.train_year,
    cust_usg.predict_year,
    cust_usg.day_of_week,
    cust_usg.week_of_year,
    cust_usg.quarter_of_year,
    cust_usg.day_of_year,
    cust_usg.day_of_year_shift,
    cust_usg.rt_sched_cd,
    cust_usg.tou_cd,
    cust_usg.usg_amt,
    cust_usg.usg_hr,
    wea_tou.wea_hr,
    wea_tou.meas_val)"
  )

  from_body <- paste0("FROM `rda`.`rdadata`.",obj_in," AS cust_usg")

  join_body <- paste0(
    "INNER JOIN `rda`.`rdadata`.",pop_in, " AS wea_tou
    ON cust_usg.day_of_year = wea_tou.day_of_year
    AND cust_usg.usg_hr = wea_tou.wea_hr
    AND cust_usg.rt_sched_cd = wea_tou.rt_sched_cd
    AND cust_usg.tou_cd = wea_tou.tou_cd"
  )

  where <- paste(
    ""
  )

  tou_body <- c(
    ""
  )

  agg_body <- c(
    "GROUP BY
    group_id,
    model_id,
    uniq_sa_id,
    deriv_baseline_terr_cd,
    opr_area_cd,
    train_year,
    predict_year,
    day_of_year,
    day_of_year_shift,
    rt_sched_cd,
    tou_cd"
    )

  create_tail <- ");"

  view_head <- paste(
    paste0("DROP VIEW IF EXISTS `rda`.`rdatables`.",obj_out),
    paste0("CREATE VIEW `rda`.`rdatables`.",obj_out), sep = ';\n')
  view_body <- paste0("AS(SELECT * FROM `rda`.`rdadata`.",obj_out, create_tail)

  cwu_query <- list(create_head, part_body, part_str, agg_head, join_head
                        , from_body, join_body, tou_body, where, agg_body,
                        create_tail, view_head, view_body)

  readr::write_lines(cwu_query, path  = out_path, append = FALSE)
}

# terr_cd <- 'X'
# usg_yr <-  2017
# pred_yr <-  2018
# tou <-  "hetoua"
# train <-  TRUE
# xy_tou_agg_sql(terr_cd)
