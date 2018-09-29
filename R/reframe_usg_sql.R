reframe_usg_sql <- function(terr_cd, usg_yr = 2017, tou = "hetoua", usg_id = "D", train = TRUE)  {

  obj_in <- stringr::str_to_lower(paste0("`",paste("usg",usg_yr, tou, terr_cd, sep = "_"),"`"))
  obj_out <- stringr::str_to_lower(paste("usg",usg_yr, tou, terr_cd, sep = "_"))
  pop_in <- stringr::str_to_lower(paste0("`",paste("model_population", terr_cd, sep = "_"),"`"))

  out_path <- paste0('data/sql/',obj_out,".sql")

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
    usg_hr,
    rt_sched_cd,
    tou_cd, usg_amt)"
    )

  part_str <-  c(
    "PARITION BY (day_of_year)")

  agg_head <-  c(
   ""
  )

  join_head <- c(
    "(SELECT cust.model_id, cust.group_id,
    cust.uniq_sa_id,
    cust.deriv_baseline_terr_cd,
    cust.opr_area_cd,
    xref.calendar_date,
    xref.month_of_year,
    xref.day_of_month,
    xref.train_year,
    xref.predict_year,
    xref.day_of_week,
    xref.week_of_year,
    xref.quarter_of_year,
    xref.day_of_year,
    xref.day_of_year_shift,
    tou.rt_sched_cd,
    tou.tou_cd,
    usg.usg_amt,
    DATE_PART('hour', usg.elec_intvl_end_dttm) AS usg_hr"
  )

  from_body <- paste0("FROM `rda`.`rdadata`.",pop_in," AS cust")

  join_body <- c(
    "INNER JOIN `rda`.`rdatables`.`elec_intvl_usg_all` AS usg
    ON cust.uniq_sa_id = usg.uniq_sa_id

    INNER JOIN  `rda`.`rdadata`.`time_shift_xref` AS xref
    ON usg.usg_dt = xref.calendar_date"
  )

  where <- paste("where usg_id =",usg_id)

  tou_body <- c(
    "JOIN  `rda`.`rdadata`.`tou_lookup_2017_hetoua` AS tou
    ON usg.usg_dt = tou.calendar_date
    AND usg.elec_intvl_end_dttm BETWEEN tou.tou_data_from_dttm AND tou.tou_data_to_dttm"
  )

  agg_body <- c(
    "")

  usg_tou_query <- list(create_head,part_body, part_str,agg_head, join_head, from_body, join_body, where, tou_body, agg_body)
  readr::write_lines(usg_tou_query, path  = out_path, append = FALSE)
  print("check output dir...")
}

reframe_usg_sql(terr_cd = "B")

terr_cd <- LETTERS[1:3]

terr_cd %>% purrr::walk(reframe_usg_sql)


