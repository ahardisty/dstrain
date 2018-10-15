options(scipen=999)

schema_in <- "`rda`.`rdadata`."
base_tbl <- "model_population"
terr_cd <- c("x","q")
obj_in <- paste(base_tbl, terr_cd, sep = "_")
model_id_min <- 1
model_id_max <- 112000
col_nms <- "group_id, model_id, uniq_sa_id, deriv_baseline_terr_cd, X, Y"

path_in <- paste0(schema_in, "`",obj_in,"`")
eval_seq <- c(1, 10, 100, 1000, seq(from = 10000, to = model_id_max, by = 10000))
base_query <- paste("SELECT",col_nms,"FROM", path_in,"WHERE group_id = 1 AND model_id <=")

eval_query <- paste(base_query, eval_seq)

eval_query[[1]]
seq(1,2,1)
