agg_pop_sql <- function() {
  setwd("~")
  proj_nm <- 'eua'
  step_num <- 3
  out_ext <- 'sql'
  dir_out <-  'data_prep'
  schema_in <- '`rda`.`rdatables`.'
  schema_out <- '`rda`.`rdadata`.'
  tbl_in <- 'cust_yearly'
  tbl_out <- "summary_population"
  agg_vals <- 'uniq_sa_id'
  alias_a <- 'cust'

  col_vals_a <- "cust.deriv_baseline_terr_cd, cust.opr_area_cd"
  col_refs_a <- col_vals_a

  step_num <- sprintf("%02d",step_num)
  obj_in <- stringr::str_to_lower(paste0(schema_in,'`',tbl_in,'`'))
  obj_out <- stringr::str_to_lower(paste0(schema_out,'`',tbl_out,'`'))


  create <- paste0("CREATE TABLE ", obj_out)

  agg_refs <- paste0("COUNT(DISTINCT ",alias_a,".",agg_vals,") AS cnt_",agg_vals)
  # col_nms <- paste0("(",paste(col_vals, agg_vals, sep = ", "),")")
  # partition <- paste0("PARTITION BY ("de")")
  conjunction <- "AS (SELECT"
  condition <- ""
  model_id <- ""
  group_id <- ""
  from <- paste("FROM",obj_in, "AS", alias_a)
  where <- paste("",condition)
  group_nm <- paste("GROUP BY", col_refs_a)

  junction <- ");"

  # condition <- paste0(alias_a,".",part_by,"=",part_seq)

  # construct query
  query_drop <- paste0("DROP TABLE IF EXISTS ", obj_out,";")
  file_nm_drop <- "00_drop_objects.sql"
  out_path_drop <- paste("../../../../mapr/shared/rda/rstudio/",proj_nm,out_ext,
                         dir_out,file_nm_drop, sep = '/')

  query_body <- paste(col_refs_a, agg_refs, sep = ", ")

  query <- paste(create, conjunction, query_body, from, where, group_nm, junction)

  file_nm <- stringr::str_to_lower(paste0(step_num, "_", tbl_out, ".",out_ext))

  out_path <- paste("../../../../mapr/shared/rda/rstudio/",proj_nm,out_ext,
                           dir_out,file_nm_create, sep = '/')

  readr::write_lines(query_drop, path = out_path_drop, append = TRUE)
  readr::write_lines(query_create, path = out_path_create, append = FALSE)

}
