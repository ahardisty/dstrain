#' model_pop_sql
#'
#' @param terr_cd
#' @param part_by
#' @param step_nm
#' @param out_ext
#' @param step_num
#'
#' @return
#' @export
#'
#' @examples
model_pop_sql <- function(terr_cd) {
  setwd("~")
  proj_nm <- "eua"
  step_num <- "03"
  out_ext <- ".sql"
  dir_out <-  'data_prep'
  schema_in <- '`rda`.`rdatables`.'
  schema_out <- '`rda`.`rdadata`.'
  tbl_in <- 'model_population'
  tbl_out <- "model_population"
  terr_cd_quo <- paste0("'",terr_cd,"'")

  obj_in <- stringr::str_to_lower(paste0("`",tbl_in,paste("", terr_cd, sep = "_"),"`"))
  obj_out <- stringr::str_to_lower(paste(tbl_in, terr_cd, sep = "_"))

  file_nm <- stringr::str_to_lower(paste0(step_num,"_", obj_out, out_ext))

  out_path <- paste("../../../../mapr/shared/rda/rstudio",proj_nm,"sql", dir_out,
                    file_nm, sep = '/')

  create_head <- paste(
    paste0("DROP TABLE IF EXISTS `rda`.`rdadata`.",obj_in),
    paste0("CREATE TABLE `rda`.`rdadata`.",obj_in), sep = ';\n')

  part_body <- c(
    "(snapshot_typ,
    snapshot_nm,
    uniq_sa_id,
    cmprs_rt_sched_cd,
    deriv_baseline_terr_cd,
    opr_area_cd,
    max_sa_dt,
    model_id,
    group_id)"
    )

  part_str <-  c(
    "PARTITION BY (group_id)")

  agg_head <-  c(
  "AS (SELECT
  snapshot_typ,
  snapshot_nm,
    uniq_sa_id,
    cmprs_rt_sched_cd,
    deriv_baseline_terr_cd,
    opr_area_cd,
    CASE WHEN sa_end_dt > CURRENT_DATE THEN CURRENT_DATE ELSE sa_end_dt END max_sa_dt,
    ROW_NUMBER() OVER (PARTITION BY deriv_baseline_terr_cd ORDER BY deriv_baseline_terr_cd) AS model_id,
    (ROW_NUMBER() OVER (PARTITION BY deriv_baseline_terr_cd)/CAST(100000 AS INTEGER)) + 1 AS group_id"
  )

  join_head <- c(
    ""
  )

  from_body <- c(
    "FROM `rda`.`rdatables`.`cust_yearly` AS cust"
  )

  join_body <- c(
    ""
  )

  where <- paste(
    "WHERE sa_sta_cd = 20
    AND snapshot_typ = 'ROLLING'
    AND res_ind = 'Y'
    AND  net_mtr_ind = 'N'
    AND cmprs_rt_sched_cd IN
    ('E1'
      ,'E1L'
      ,'E1'
      ,'E1L'
      ,'E6'
      ,'ETOUA'
      ,'ETOUB'
      ,'E6L'
      ,'ETOUAL'
      ,'ETOUP1L'
      ,'ETOUP1'
      ,'ETOUP2'
      ,'ETOUP3'
      ,'ETOUP2L'
      ,'ETOUP3L'
      ,'ETOUBL'
      ,'E6'
      ,'E6L'
      ,'ETOUB'
      ,'ETOUA'
      ,'ETOUAL'
      ,'ETOUBL'
    )
    AND deriv_rt_sched_cd NOT LIKE
    (
      'SE%'
    )
    AND deriv_baseline_terr_cd = ", terr_cd_quo)

  tou_body <- c(
    ""
  )

  agg_body <- c(
    "LIMIT 10")

  create_tail <- ");"

  view_head <- paste(
    paste0("DROP VIEW IF EXISTS `rda`.`rdatables`.",obj_in),
    paste0("CREATE VIEW `rda`.`rdatables`.",obj_in), sep = ';\n')
  view_body <- paste0("AS(SELECT * FROM `rda`.`rdadata`.",obj_in, create_tail)

  model_pop_query <- list(create_head, part_body, part_str, agg_head, join_head
                          , from_body, join_body, tou_body, where, agg_body,
                          create_tail, view_head, view_body)

  readr::write_lines(model_pop_query, path  = out_path, append = FALSE)

}
