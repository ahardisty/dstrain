#' get_training
#'
#' @param con a database connection; this can be in memory or remote
#' @param db_schema name of schema where data is located
#' @param db_tbl name of table where data is located
#' @param model_id a number assigned to each member of the model population (n = 1*10)
#' @param group_id a number assigned to a group of the model population (n = 100,000)
#'
#'
#'
#'
#'
#' @return a promise of data from a remote database
#' @export
#'
#' @examples
#' soon enough
#'
#'
get_training <- function(con = con
                         , db_schema
                         , db_tbl
                         , model_id = 1L
                         , group_id = 1L
                         ) {
  schema_tbl <- paste(db_schema, db_tbl, sep = ".")
  query_string <- paste("SELECT uniq_sa_id, deriv_baseline_terr_cd, opr_area_cd
                        , model_id, group_id, rt_sched_cd, X, Y FROM",
                        schema_tbl,"LIMIT 10")

  lkh <- cust
  return(lkh)
}
