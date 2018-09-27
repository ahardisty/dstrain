usg_elec_sql <- function(terr_cd, part_by, step_nm, step_num, out_ext) {
  step_num <- sprintf("%03d",step_num)
  terr_cd_quo <- paste0("'",terr_cd,"'")
  obj_base <- "usg"

  out_dir = 'data_prep'
  schema <- '`rda`.`rdadata`.'
  tbl_nm <- stringr::str_remove_all(string = obj_base ,pattern = out_ext)
  obj_nm <- stringr::str_to_lower(paste0(schema,'`',tbl_nm,terr_cd,'`'))

  drop <- paste0("DROP TABLE IF EXISTS ", obj_nm,";")
  create <- paste0("CREATE TABLE ", obj_nm)
  col_vals <- stringr::str_replace_all(string = "snapshot_typ, snapshot_nm, uniq_sa_id, cmprs_rt_sched_cd, deriv_baseline_terr_cd, opr_area_cd",pattern = "[\r\n]", replacement = "")
  agg_vals <- paste("max_sa_dt","model_id", "group_id", sep = ", ")

  col_nms <- paste0("(",paste(col_vals, agg_vals, sep = ", "),")")
  partition <- paste0("PARTITION BY (",part_by,")")
  conjunction <- "AS (SELECT"
  condition <- "CASE WHEN sa_end_dt > CURRENT_DATE THEN CURRENT_DATE ELSE sa_end_dt END max_sa_dt"
  model_id <- "ROW_NUMBER() OVER (PARTITION BY deriv_baseline_terr_cd) AS model_id"
  group_id <- "(ROW_NUMBER() OVER (PARTITION BY deriv_baseline_terr_cd)/CAST(100000 AS INTEGER)) + 1 AS group_id"
  from <- "FROM `rda`.`rdatables`.`cust_yearly` AS cust"
  where <- stringr::str_replace_all(string = paste("WHERE sa_sta_cd = 20 AND snapshot_typ = 'ROLLING' AND res_ind = 'Y' AND net_mtr_ind = 'N' AND cmprs_rt_sched_cd IN('E1','E1L','E6','E6L','ETOUA','ETOUAL','ETOUB','ETOUBL','ETOUP1','ETOUP1L','ETOUP2','ETOUP2L','ETOUP3','ETOUP3L') AND deriv_rt_sched_cd NOT LIKE ('SE%') AND deriv_baseline_terr_cd ="
                                                   ,terr_cd_quo), pattern = "[\r\n]", replacement = "")
  junction <- ")"

  # tbl_obj <- paste0(schema, tbl_nm)
  file_nm <- stringr::str_to_lower(paste0(step_num, "_", tbl_nm, terr_cd))
  out_ext <- stringr::str_remove_all(string = out_ext ,pattern = tbl_nm )
  file_nm <- paste(file_nm, out_ext, sep = ".")
  out_dir <- paste(getwd(),out_ext, step_nm, sep = '/')

  # checking for directory
  if(dir.exists(out_dir) == FALSE){
    dir.create(out_dir, recursive = TRUE)
    print(paste0("required directory not present; creating"," '",out_dir,"'"))

  } else {
    print(paste0("required directory"," '",out_dir,"' ","already present"))
  }

  out_path <- paste(out_dir, file_nm, sep = '/')
  # construct query
  query_body <- paste(col_vals, condition, model_id, group_id, sep = ", ")
  query <- paste(drop, create, col_nms, partition, conjunction, query_body, from, where, junction)

  # save query as file
  readr::write_lines(query, path = out_path)
}
