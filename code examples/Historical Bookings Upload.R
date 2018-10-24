# 00a: INSTALL AND LOAD PACKAGES --------------------------------------------
install.packages("stringr")
install.packages('tidyverse')
install.packages('stringr')
install.packages('readxl')
install.packages('data.table')
library(tidyverse)
library(stringr)
library(readxl)
library(data.table)
library(purrr)


# 00b workflow example --------------------------------------------------------


# http://readxl.tidyverse.org/articles/articles/readxl-workflows.html

# path <- readxl_example("datasets.xlsx")
# path <- fileList[[1]] 

path <- readxl_example("datasets.xls")
sheets <- excel_sheets(path)
sheets <- map(.x = path, .f = excel_sheets)

xl_list <- lapply(excel_sheets(path), read_excel, path = path) 
names(xl_list) <- sheets

read_then_csv <- function(sheet, path) {
  pathbase <- path %>%
    basename() %>%
    tools::file_path_sans_ext() %>% 
    map2(.y = "_", .f = ~tolower(gsub(x = .x, replacement = .y, pattern = " "))) %>% 
    
    path %>%
    read_excel(sheet = sheet[[1]]) %>%
    write_csv(paste0(pathbase, "-", tolower(gsub(pattern = " ", replacement = "_", x = sheet[[1]])), ".csv"))
}

# th <- read_then_csv(sheet = sheetList[[1]][1], path = path)

path <- readxl_example("datasets.xlsx")
y <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_then_csv, path = path) %>% 
  bind_rows() %>% 
  set_names() %>% 
  map(set_names)
names(y)

path %>% 
  excel_sheets() %>% 
  set_names() %>%
  map(.f = ~read_then_csv(path = path, ))



path%>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(paste0)
names(fileList)
# readxl::read_xlsx(path = fileList[[1]], sheet = sheetList[[1]][1])
readlist <- map2(.x = fileList[[1]], .y = sheetList[[1]], .f = readxl::read_excel)
readlist <- map2(.x = fileList, .y = sheetList, .f = readxl::read_xlsx)
readlist <- map(.x = fileList, .f = readxl::read_xlsx)
readlist[2]
fileList[1]
rm(readlist)
paste(sheetList[[1]], collapse = ",")




# 01: LOAD FUNCTIONS -------------------------------------------------------------
# check for and create directories
mk_dirs <- function(dir_nm) {
  if(dir.exists(dir_nm) == FALSE){
    dir.create(dir_nm, recursive = TRUE)
    print(paste("creating required file path",dir_nm))
    
  } else {
    print(paste(dir_nm,"file path already present"))
  }
}

# read in XLSX files to convert to .csv
read_reorder <- function(filename, ...) {
  readxl::read_excel(filename, trim_ws = T, n_max = ..., sheet = 1) %>%
    # data.table::fread(input = filename, nrows = ..., strip.white = T) %>%
    dplyr::select(order(colnames(.))) %>% 
    dplyr::mutate(source = basename(filename))
}


# read in FLAT files to convert to .csv
read_reorder_flat <- function(filename, ...) {
  # readxl::read_excel(filename, trim_ws = T, n_max = ...) %>%
  data.table::fread(input = filename, nrows = ..., strip.white = T) %>%
    dplyr::select(order(colnames(.))) %>% 
    dplyr::mutate(source = basename(filename))
}

# format objects in memory - rename and reorder columns
format_cols <- function(df, pattern_1 = "[\\W+.-]+", pattern_2 = "[_]+\\b", replacement_1 = "_", replacement_2 = ""){
  set_names(df, stringr::str_replace_all(string = colnames(df)
                                         , pattern = pattern_1
                                         , replacement = replacement_1) %>% 
              tolower()) %>% 
    set_names(stringr::str_replace_all(string = colnames(.)
                                       , pattern = pattern_2
                                       , replacement = replacement_2)) %>%
    dplyr::select(order(colnames(.)))
}

# replace NA with 0
f_dowle3 = function(DT) {
  # either of the following for loops
  
  # by name :
  # for (j in names(DT))
  #   set(DT,which(is.na(DT[[j]])),j,0)
  # 
  # or by number (slightly faster than by name) :
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}

# 02: CREATE LIST OF FILES TO MANIPULATE --------------------------------------------------------------------
# set working directory to home
setwd("~")
mk_dirs('Desktop/Uploads') # create data source directory
mk_dirs('Desktop/csv_conversion') # create data location directory

# data_source <- "the/path/where/the/files/are.xlsx|csv|txt"
data_source <- 'Desktop/Uploads'

# CREATE XLSX FILE LIST
fileList_xlsx <- dir(path = data_source, recursive = T, full.names = T, pattern = '(.xlsx)') # list of XLSX files WITH FULL NAMES
fileNames_xlsx <- dir(path = data_source, recursive = T, full.names = F, pattern = '(.xlsx)') # list of XLSX files WITHOUT FULL NAMES

# CREATE .csv and .txt FILE LIST including BigQuery tables
fileList_flat <- dir(path = data_source, recursive = T, full.names = T, pattern = '(.txt|.csv)') # list of FLAT files WITH FULL NAMES
fileNames_flat <- dir(path = data_source, recursive = T, full.names = F, pattern = '(.txt|.csv)') # list of FLAT files WITHOUT FULL NAMES

# ALL FILES REGARDLESS OF TYPE
fileList_finance <- dir(path = data_source, recursive = T, full.names = F)
fileList <- dir(path = data_source, recursive = T, full.names = F)

length(fileList_finance) # CHECK LENGTH (NUMBER OF FILES) from finance
length(fileList) # CHECK LENGTH (NUMBER OF FILES) including BQ files

# create reference sheet group by category / object
reference_df <- fileList %>% purrr::map_df(as.data.frame) %>% # create reference sheet for files converted to csv, this will be used to determine groupings
  purrr::set_names('Source_Full') %>% 
  dplyr::mutate(Category = '',
         sub_category = '',
         Summary = '',
         Time_Period = '',
         Source_Short = Source_Full %>% basename() %>% tools::file_path_sans_ext(),
         csv_conversion = stringr::str_replace_all(basename(Source_Full), pattern = "[\\W+.-]+", replacement = "_") %>%
           tolower() %>% 
           paste0('.csv'),
         Source_Type = tools::file_ext(Source_Full) ,
         Review_Round = Sys.Date()) %>% 
  dplyr::select(Category, sub_category,	Time_Period,	Source_Short,	csv_conversion, Source_Full, Source_Type,	Review_Round ) %>% 
  dplyr::arrange(Source_Short)
  
# write reference sheet to data_location
write.csv(x = reference_df, file = paste(data_source,"Reference_Sheet.csv", sep = '/'),  append = T) # this creates a blank csv worksheet to be added to Reference_Sheet.xlsx

# 03: READ IN SOURCE FILES TO CONVERT TO CSV----------------------------------------------------
# read xlsx files to create a list of data tables (faster to manipulate)
# make note of warnings. They will not stop the function but might indicate missing or corrupted data
xlsx_list <- purrr::map(.x = fileList_xlsx, .f = ~read_reorder(filename = .x) %>% # pass function to list
                   data.table) %>% # convert to data.table
  purrr::set_names(basename(fileList_xlsx)) # name elements in list for easier reference

# read txt and csv files to create a list of data tables
# make note of warnings. They will not stop the function but might indicate missing or corrupted data
flat_list <- purrr::map(.x = fileList_flat, .f = ~read_reorder_flat(filename = .x)) %>% # pass function to list, data.table by default
  purrr::set_names(basename(fileNames_flat)) # name elements in list for easier reference

# view the first six rows of each data table in the xlsx list
xlsx_list %>% purrr::map(head)
# view the first six rows of each data table in the flat file list
flat_list %>% purrr::map(head)

# 04: COMBINE FLAT FILES AND CSV FILES; COUNT ROWS AND COLUMNS BEFORE AND AFTER RECONCILIATION -----------------------
full_list <- c(xlsx_list, flat_list) # combine lists of data tables
full_list <- full_list[names(full_list) %>% order] # reorder data table list

# create filesnames for csv conversion
fileNames <- full_list %>% names() %>% # extract names of elements of each list (not column names)
  stringr::str_replace_all(pattern = "[\\W+.-]+", replacement = "_") %>% # replace non-word characters with "_"
  tolower() %>% # make lower case for consistent indexing
  paste0('.csv') # paste ".csv" for saving file name

# check dimensions of renamed and reordered files (before removing)
# row count BEFORE reconciliation
row_count <-  full_list %>% 
  purrr::map(. %>% nrow) %>% # create row count
  dplyr::bind_rows() %>% 
  tidyr::gather(key = "finance_file_name", value = "finance_row_count")

# column count BEFORE column header reconciliation
col_count <-  full_list %>% 
  purrr::map(. %>% dplyr::select(-source)) %>%  # don't count added source column
  purrr::map(. %>% NCOL) %>% # create col count
  dplyr::bind_rows() %>% 
  tidyr::gather(key = "finance_file_name", value = "finance_column_count_no_removals")

# column count of columns marked as remove BEFORE column header reconciliation
col_count_remove <-  full_list %>% 
  purrr::map(. %>% dplyr::select(dplyr::starts_with("remove"))) %>%  # remove columns with "remove"
  purrr::map(. %>% NCOL) %>% # create col count
  dplyr::bind_rows() %>% 
  tidyr::gather(key = "finance_file_name", value = "num_marked_for_removal")

bind_count <- cbind(row_count, col_count[-1], col_count_remove[-1]) # column bind dimensions
# write row_count
write.csv(x = bind_count, file = paste(data_location,"source_file_count_original.csv", sep = '/'), row.names = FALSE)

# remove any variables with "remove" in action
full_list <- full_list %>% 
  purrr::map(. %>% dplyr::select(-dplyr::starts_with("remove"))) # remove columns with "remove"

full_list <- full_list %>% 
  purrr::map(. %>% dplyr::select(-dplyr::matches("X__"))) # remove columns blanks (pivots)

# row count AFTER reconciliation
row_count <-  full_list %>% 
  purrr::map(. %>% nrow) %>% # create row count
  dplyr::bind_rows() %>% 
  tidyr::gather(key = "finance_file_name", value = "finance_row_count")

# column count AFTER column header reconciliation
col_count <-  full_list %>% 
  purrr::map(. %>% dplyr::select(-source)) %>%  # exclude source column
  purrr::map(. %>% NCOL) %>% # create col count
  dplyr::bind_rows() %>% 
  tidyr::gather(key = "finance_file_name", value = "column_count_after_removal")

# combine to create summary for post reconciliation output
bind_count <- cbind(row_count, col_count[-1]) # column bind dimensions

# write row_count
write.csv(x = bind_count, file = paste(data_location,"source_file_count_reconciled.csv", sep = '/'), row.names = FALSE)
# ignore warnings

# additional function: select element items (data tables) by specific column names
full_list %>% 
  purrr::map(. %>% dplyr::select(dplyr::contains("discount"))) # select by specific column(s)

# 05: SAVE RECONCILED FILES AS CONVERTED CSV ----------------------------------
# save ordered files as csv_conversion files
purrr::walk2(.x = full_list, .y = paste0('Desktop/csv_conversion/',fileNames)
      , .f = ~readr::write_csv(x = .x, path = .y, col_names = T))

# 7: READ IN OBJECT REFERENCE SHEET AND CONVERTED DATA SOURCES ---------------------
# read in completed reference sheet
reference_sheet <- read.csv('Desktop/Uploads/Reference_Sheet.csv') # read in mapping schema

# DETERMINE DESTINATION OBJECT NAMES
group_type <- reference_sheet %>% 
  dplyr::select(sub_category) %>% 
  dplyr::group_by(sub_category) %>% 
  dplyr::summarise() %>% 
  unlist(use.names = FALSE)

# split list based on destination object name
nm <- reference_sheet %>% # read finance mapping worksheet
  dplyr::select(csv_conversion, Source_Short, Category, sub_category) %>%
  # filter(Category == "Expiring One-Time Disount") %>%
  # split(list(.$Category)) %>% split on category
  split(list(.$sub_category)) # split on SOURCE
# split(list(.$Category)) # split on SOURCE
# filter(Category == 'Book of Business') %>% 

# assign location of each file within its destination object name group (sub-category)
# files <- nm %>% map(. %>% select(one_of('csv_conversion'))) %>% 
files <- nm %>% purrr::map(. %>% select(one_of('csv_conversion'))) %>% 
  purrr::map(. %>% dplyr::mutate(file_loc = paste0(colnames(.), "/",(.$csv_conversion))))%>% 
  # map(. %>% select(file_loc))
  purrr::map(. %>% dplyr::select(file_loc))

new_dir <- "gcs_files" # assign source directory for combined object
mk_dirs(new_dir)

# setwd("Talk/")
full_files <- purrr::map(files$Bookings$file_loc, data.table::fread)

joined_full_files <- rbindlist(full_files, fill = TRUE)

# full_files <- map(files$`Book of Business`$file_loc, fread)
# full_files <- map(files$`Book of Business_Aggregated`$file_loc, fread)
# full_files <- map(files$Bookings$file_loc, fread)
# full_files <- map(files$Bookings_Final$file_loc, fread)
# full_files <- map(files$`Churn/Contraction`$file_loc, fread)
# full_files <- map(files$`Churn/Contraction_BIME`$file_loc, fread)
# full_files <- map(files$`Churn/Contraction_BIME`$file_loc, fread)
# 
# 
# full_files <- map(files$`Expiring One-Time Discount`$file_loc, fread)

# full_files <- files[[1]]$file_loc %>% map(. %>% fread) %>% 
#   data.table::rbindlist(fill =  T) 
# map(. %>% select(file_loc))


full_files %>% purrr::map(colnames)
full_files %>% purrr::map(NROW)
full_files %>% purrr::map(head)
full_files %>% purrr::map(ncol) 

joined_full_files %>% colnames()
joined_full_files %>% NROW()
joined_full_files %>% head()
joined_full_files %>% ncol() 



# f_list <- full_files %>% map(. %>% select(one_of(keep_col))) 
f_list <- full_files %>% purrr::map(. %>% select(contains('product')))
f_list <- full_files %>% purrr::map(. %>% select(contains('Agent')))

# try to rename if 
# https://stackoverflow.com/questions/30382908/r-dplyr-rename-variables-using-string-functions



# head(joined_full_files)
# to_date[2:3]
# to_date<- colnames(joined_full_files)[colnames(joined_full_files)%like%'[Dd]ate'] # for all files
# joined_full_files[, c(to_date[2:3]) := lapply(.SD, as.IDate, format = "%Y-%m-%d" ), .SDcols = c(to_date[2:3])] # for book of business aggregated.xlsx (get rid of billing_region_updated)
# joined_full_files[, c(to_date,'term_start','month') := lapply(.SD, as.IDate), .SDcols = c(to_date, 'term_start','month')]
# # joined_full_files_2[,mrr_date:= as.IDate(mrr_date, format = "%m/%d/%Y")]


# setnames(joined_full_files, "Lead Date", "lead_date") #
# setnames(joined_full_files, "# of Agents", "Agents") #
# head(joined_full_files)
# columns to change to date type


# summary(as.factor(joined_full_files$campaign))
f_dowle3(joined_full_files) # make na into 0


current_name <- which(group_type == 'Bookings') #churn_contraction
# current_name <- which(group_type == 'Book of Business_Aggregated') #book of business aggregated
# current_name <- which(group_type == 'Book of Business_Aggregated') #book of business aggregated
# current_name <- which(group_type == 'Book of Business_Aggregated') #book of business aggregated
# current_name <- which(group_type == 'Book of Business_Aggregated') #book of business aggregated
# current_name <- which(group_type == 'Book of Business_Aggregated') #book of business aggregated

# write_csv(x = joined_full_files, path = paste(getwd(),new_dir,group_type[[6]],sep="/"), col_names = T)

write_csv(x = joined_full_files, path = paste(getwd(),new_dir,paste0(group_type[current_name],'.csv'),sep="/"), col_names = T)

# write_csv(x = joined_full_files, path = paste(getwd(),paste0(group_type[current_name],'.csv'),sep="/"), col_names = T)

# https://stackoverflow.com/questions/34725617/rename-columns-from-a-lookup-table


# IGNORE: pass rename to list -----------------------------------------------------



# book of business
nm <- name_mapping %>% # read finance mapping worksheet
  dplyr::select(ORIGINAL_NAME, SOURCE, Action, Category) %>%
  dplyr::filter(Action != 'remove') %>% 
  dplyr::filter(Category == 'Bookings') %>%
  dplyr::arrange(SOURCE, ORIGINAL_NAME) %>% 
  split(list(.$SOURCE)) %>%
  purrr::map(. %>% dplyr::group_by(ORIGINAL_NAME, Action, Category) 
      %>% dplyr::summarise()) %>%
  purrr::map(. %>% dplyr::filter(Action != 'remove') %>% 
               dplyr::ungroup())




old_col <- c(nm$`Book of Business`$ORIGINAL_NAME,'source')
new_col <- c(nm$`Book of Business`$Action, 'source')

which(colnames(combine_files_book_of_business1)%in%old_col)
new_col[colnames(combine_files_book_of_business1)%in%old_col]
?at_depth
combine_files_book_of_business1 <- rbindlist(new_files[1:7], fill = TRUE)
setnames(combine_files_book_of_business1,old = old_col, new = new_col) # set names
combine_files_book_of_business1 <- combine_files_book_of_business1[, new_col, with = FALSE] # select

combine_files_book_of_business2 <- rbindlist(new_files[8:length(new_files)], fill = TRUE)
old_col_alt <- c('account_number','region_name','mrr_amt','max_agents')
new_col_alt <- c('support_id','billing_region','mrr','number_of_max_agents')
setnames(combine_files_book_of_business2, old_col_alt, new_col_alt)

combine_book_of_business <- rbind(combine_files_book_of_business1, combine_files_book_of_business2, fill = TRUE)

combine_book_of_business <- combine_book_of_business[, new_col, with = FALSE] # select

# bookings
old_col <- c(nm$`Bookings`$ORIGINAL_NAME,'source')

nm$`Bookings` %>% filter(ORIGINAL_NAME == 'Region')
new_col <- c(nm$`Bookings`$Action, 'source')
colnames(new_files[[2]])

pmap(.l = list(x = list(new_files[[1]]), old = old_col, new = new_col), setnames)
?pmap
setnames(x = asd, old = , new =)

combine_files_bookings <- rbindlist(new_files[[2]], fill = TRUE)
setnames(combine_files_bookings,old = old_col, new = new_col) # set names
combine_files_book_of_business1 <- combine_files_book_of_business1[, new_col, with = FALSE] # select

combine_files_book_of_business2 <- rbindlist(new_files[8:length(new_files)], fill = TRUE)
old_col_alt <- c('account_number','region_name','mrr_amt','max_agents')
new_col_alt <- c('support_id','billing_region','mrr','number_of_max_agents')
setnames(combine_files_book_of_business2, old_col_alt, new_col_alt)

combine_book_of_business <- rbind(combine_files_book_of_business1, combine_files_book_of_business2, fill = TRUE)

combine_book_of_business <- combine_book_of_business[, new_col, with = FALSE] # select

old_col <- nm$`Expiring One-Time Discount`$ORIGINAL_NAME
new_col <- nm$`Expiring One-Time Discount`$Action

old_col <- nm$`Expiring One-Time Discount`$ORIGINAL_NAME
new_col <- nm$`Expiring One-Time Discount`$Action

old_col <- nm$`Expiring One-Time Discount`$ORIGINAL_NAME
new_col <- nm$`Expiring One-Time Discount`$Action









combine_files_discounts <- combine_files[!is.na(Region),`Owner Region`:=Region][, old_col, with = FALSE]
setnames(combine_files,old = colnames(combine_files), new = new_col) # discounts


rm(check_change)
str(keep_col)
colnames(test_files[[10]])
?setnames



# new_col <- c('blue','black','yellow')
# setnames(x = df_dt, old = keep_col, new = new_col)
# 



# map(.x = df_dt, .f = str)
# map(.x = nm, .f = str, max.level = 1)
# ?str
# str(nm, max.level = 1)

# map to 
?one_of
# pass select to list

# df_list[[1]] %>% select(one_of(keep_col))
# df_list %>% map(select(one_of(keep_col)))
# map(.x = df_list, .f = select(one_of(keep_col)))
f_list <- new_files %>% map(. %>% select(one_of(keep_col))) # retain selected columns
fileNames <- full_list %>% map(.f = names) # uninteded outcome: provides names of all list, try to get the df names instead

listlen <- f_list %>% map(length) # get number of columns
listrow <- f_list %>% map(nrow) # get number of rows

f_list <- f_list[which(listlen != 0)]

f_list %>% map(. %>% filter(account_id == '0018000001LeWs1'))
map(. %>% names(.)) # gets names from elements lists, not what we want




# pass rename to list
# https://csgillespie.github.io/efficientR/5-5-dplyr.html

# The dplyr way (rename two variables)
# idata <- rename(idata,
#                 top10 = `Income share held by highest 10% [SI.DST.10TH.10]`,
#                 bot10 = `Income share held by lowest 10% [SI.DST.FRST.10]`)
# 
# csv_list <- 

xlsx_list <- map(.x = fileList_xlsx[one_time_discount], .f = ~read_reorder(filename = .x, n_max = 2) %>% 
                   data.table) %>% 
  # map(remove_pivots) %>% 
  # map(format_cols) %>% 
  set_names(fileNames_xlsx[one_time_discount])# make it a loop

xlsx_list %>% map(str, max.level = 1)
xlsx_list %>% map(length)
length(xlsx_list)
colnames(xlsx_list[[1]])
colnames(xlsx_list[[10]])



# split(x = name_mapping, list(.$SOURCE)) %>% 
# name_mapping$Concat


str(nm)  
map(nm, .f = str())

# length(nm)
# nm$`Expiring Discounts Final Q1 2017`[,SOURCE:= stringr::str_replace_all(SOURCE, pattern = "[\\W+.-]+", replacement = "_")]
# rename_all
# rename_if
# rename_at

# https://gist.github.com/djhocking/62c76e63543ba9e94ebe

# select(df2, one_of(names.df))    # success
# select(df2, one_of(names.df.2))  # success

# additional functions ----------------------------------------------------


# create schema for files
find_schema_df_list <- function(filename) {
  y <- readxl::read_excel(filename, n_max = 1, trim_ws = T) %>%
    at_depth(1, 1) %>% at_depth(1, class) %>%
    flatten_df() %>% 
    gather(key = variable_name, value = value) %>%
    mutate( variable_name2 = gsub(x = variable_name, replacement = "_", pattern = " ")) %>%
    mutate( variable_name2 = gsub(x = variable_name2, replacement = "", pattern = "_._")) %>% 
    mutate( variable_name2 = gsub(x = variable_name2, replacement = "", pattern = "[()]")) %>% 
    mutate( variable_name2 = gsub(x = variable_name2, replacement = "", pattern = "_%")) %>% 
    mutate(alt_value = "STRING") %>% 
    mutate(sc = paste0(variable_name2, ":", alt_value, collapse = ",")) %>% 
    select(sc) %>% 
    head(1) %>% 
    flatten_chr() %>% 
    gsub(replacement = "_", pattern = " ") %>% 
    gsub(replacement = "", pattern = "\\/") %>% 
    data.frame()
}

y %>% arrange(desc(variable_name)) %>% filter(variable_name2 %in% 
                                                c('Total_Guide_MRR_converted_Currency',
                                                  'Zendesk_Software_Discount'))

schemanames <- fileList %>% map(.f = basename) %>% tools::file_path_sans_ext() %>% 
  gsub(replacement = " ", pattern = ' - ') %>% 
  gsub(replacement = "", pattern = '[()]') %>% 
  gsub(replacement = "_", pattern = ' ') %>% 
  paste0('_schema') %>% 
  tolower() %>% 
  paste0('.txt')

headernames <- fileList %>% map(.f = basename) %>% tools::file_path_sans_ext() %>% 
  gsub(replacement = " ", pattern = ' - ') %>% 
  gsub(replacement = "_", pattern = ' ') %>% 
  gsub(replacement = "", pattern = "\\W") %>% 
  paste0('_header') %>% 
  tolower() %>% 
  paste0('.csv')

schema <- map(.x = fileList, .f = find_schema_df_list)
walk2(.x = schema, .y = paste0(getwd(),'/csv_conversion/',schemanames), .f = ~write_csv(x = .x, path = .y, col_names = F))



# extract and save for column reconciliation
rm(dfList)
headerList <- map(.x = fileList, .f = read_one_sheet) %>% bind_rows()

write_csv(x = headerList, path = 'headerlist.csv', col_names = T)


# scratch -----------------------------------------------------------------


# https://stackoverflow.com/questions/22850026/filtering-row-which-contains-a-certain-string-using-dplyr

fileList[which(fileList == "/Users/ahardisty/Box Sync/Super Project Files/Support Bookings 2016 2017 Q2 (Final).xlsx")]

str(fileList)
x <- map(.x = fileList[which(fileList == "/Users/ahardisty/Box Sync/Super Project Files/Support Bookings 2016 2017 Q2 (Final).xlsx")], .f = read_excel_allsheets)

ALL_PLOTS <- x %>%
  map(. %>% filter(Team == 'BrickSquad')) %>%
  map(. %>% filter(str_detect(Team, pattern)==TRUE)) %>% 
  
  
  
  
  x <- x %>% bind_rows() %>% filter(stringr::str_detect(pattern = "[0-9]", string = variable_name)==FALSE)
x <- x[[1]] %>% filter(stringr::str_detect(pattern = "[0-9]", string = variable_name)==TRUE)
x <-  filter(x[[1]], stringr::str_detect(pattern = "[0-9]", string = variable_name)==TRUE)
x %>% x[1]$sheet == 'Sheet2')

str(x, max.level = 2)

y <- x[1] %>% at_depth(1, 0) %>%  set_names("blue")
%>% set_names("blue")
x[[1]]
filename <- fileList[[1]]



x[[1]] %>% filter(stringr::str_detect(pattern = "[AaZz]", string = variable_name)==F)
filter(x[[1]], stringr::str_detect(pattern = "[0-9]", string = variable_name)==TRUE)


x[1] %>% map( .f = ~stringr::str_detect(pattern = "[0-9]", string = .$Name))
map(.x = x[1], .f = ~stringr::str_detect(pattern = "[0-9]", string = .$Name))

map(.x = x[1:2], .f = ~stringr::str_detect(pattern = "[0-9]", string = .$variable_name))
map(.x = x[1:2], .f = ~stringr::str_detect(pattern = "[0-9]", string = .$variable_name))

plot_list <- map2(.x = gap_split_small, .y = countries
                  , .f =  ~ggplot(.x, aes(x = .$year, y = .$lifeExp)) +
                    geom_line() +
                    labs(title = .y)
)

# map(.f = filter(stringr::str_detect(pattern = "[0-9]", string = variable_name)==TRUE))


save_path <- function(x){
  basename(x) %>%
    tools::file_path_sans_ext() %>% 
    map2(.y = "_", .f = ~tolower(gsub(x = .x, replacement = .y, pattern = " "))) %>% 
    paste0('.csv')
}
save_path(x = fileList[[3]]) 
paths <- map(.x = fileList, .f = save_path)
str(x, max.level = 1)
str(paths, max.level = 1)
walk2(.x = x, .y = paths, .f = write.csv)
save_path(path = fileList[[1]]) 

y %>% map2(.f = set_names, nm = y %>% map( "source"))
y <- map(.x = fileList[1], .f = find_class_df_list)
set_names(x = y[1], nm = "d")
# %>% map(bind_rows) %>% 


# m <- x[1][1] %>% at_depth(2, 1) %>% at_depth(2, class)
# m <- x %>% at_depth(2, 1) %>% at_depth(2, class)
# m <- x %>% at_depth(2, 1) %>% at_depth(2, class) %>% map(data.frame)
# df <- y[[1]]
# df <- y[[1]][[1]]
# class(flatten(df))
# sapply(df, class)
# y[[1]] %>% map(class)
# 
# y[[1]] %>% map(class)
# 
# set.seed(123)
# thelist <- list(a=data.frame(x1=rnorm(10), x2=rnorm(10)),
#                 b=data.frame(x1=rnorm(10), x2=rnorm(10)),
#                 c=data.frame(x1=rnorm(10), x2=rnorm(10)))
# 
# x %>% filter(stringr::str_detect(pattern = "[0-9]", string = variable_name)==FALSE) 
# map(x[[1]], class)

# map(x,~class(.x))






# Lecture Examples --------------------------------------------------------

library(repurrrsive)
gap_split_small <- gap_split[1:10]
library(ggplot2)
library(purrr)
countries <- names(gap_split_small)


ggplot(data = gap_split_small[[1]], aes(x = year, y = lifeExp)) +
  geom_line() +
  labs(title = countries[[1]])

plot_list <- map2(.x = gap_split_small, .y = countries
                  , .f =  ~ggplot(.x, aes(x = .$year, y = .$lifeExp)) +
                    geom_line() +
                    labs(title = .y)
)
str(plot_list)

ggsave(filename = paste0(countries[[1]],'.pdf'), plot = plot_list[[1]])

map2(.x = countries, .y = plot_list, .f = ~ ggsave(filename = paste0(.x,".pdf"),plot = .y))
walk2(.x = countries, .y = plot_list, .f = ~ ggsave(filename = paste0(.x,".pdf"),plot = .y))
walk2(.x = countries, .y = plot_list, .f = ~  unlink(x = paste0(.x,".pdf"))) #remove


walk2(.x = plot_list, .y = , .f = ggsave(filename = .y))



map2(.f = ggplot(data = .x, aes(x = .$year, y = .$lifeExp)) +
       geom_line()) +
  labs(title = .y)


char_starships <- map(sw_people, "starships")
map_int(char_starships, length)
map(char_starships, class)
# In one go
map(sw_people, "starships") %>% map_int(length)
# also equivalent to
map_int(sw_people, ~ length(.x[["starships"]]))

planet_lookup <- map_chr(sw_planets, "name")  %>%
  set_names(map_chr(sw_planets, "url"))
planet_lookup
sw_species[[1]]$eye_colors

map(got_chars[5], 3)

x <- list(a = 1:4, b = letters[5:7], c = 8:9, d = letters[10])

# https://cran.r-project.org/web/packages/purrr/purrr.pdf

l1 <- list(
  obj1 = list(
    prop1 = list(param1 = 1:2, param2 = 3:4),
    prop2 = list(param1 = 5:6, param2 = 7:8)
  ),
  obj2 = list(
    prop1 = list(param1 = 9:10, param2 = 11:12),
    prop2 = list(param1 = 13:14, param2 = 15:16)
  )
)

# In the above list, "obj" is level 1, "prop" is level 2 and "param"
# is level 3. To apply sum() on all params, we map it at depth 3:
l1 %>% at_depth(3, sum)
l1 %>% at_depth(3, class)
m <- y[[1]][1] %>% at_depth(2, 1) %>% at_depth(2, class) 
str(data.frame(m))
l1[1]
# In the above list, "obj" is level 1, "prop" is level 2 and "param"
# is level 3. To apply sum() on all params, we map it at depth 3:
l1 %>% at_depth(3, sum)
# map() lets us pluck the elements prop1/param2 in obj1 and obj2:
l1 %>% map(c("prop1", "param2")) %>% str()
# But what if we want to pluck all param2 elements? Then we need to
# act at a lower level:
l1 %>% at_depth(2, "param2") %>% str()
# at_depth can be used in a complementary way with other purrr
# functions to make them operate at a lower level
l2 <- list(
  obj1 = list(
    prop1 = list(c(1, 2), c(3, 4), c(5, 6)),
    prop2 = list(c("a", "b"), c("c", "d"), c("e", "f"))
  ),
  obj2 = list(
    prop1 = list(c(10, 20), c(30, 40), c(50, 60)),
    prop2 = list(c("A", "B"), c("C", "D"), c("E", "F"))
  )
)
# Here we ask pmap() to map paste() simultaneously over all
# elements of the objects at the second level. paste() is thus
# effectively mapped at level 3.
l2 %>% at_depth(2, pmap, paste, sep = " / ")
l2 <- list(
  obj1 = list(
    prop1 = list(c(1, 2), c(3, 4), c(5, 6)),
    prop2 = list(c("a", "b"), c("c", "d"), c("e", "f"))
  ),
  obj2 = list(
    prop1 = list(c(10, 20), c(30, 40), c(50, 60)),
    prop2 = list(c("A", "B"), c("C", "D"), c("E", "F"))
  )
)
# Here we ask pmap() to map paste() simultaneously over all
# elements of the objects at the second level. paste() is thus
# effectively mapped at level 3.
l2 %>% at_depth(2, pmap, paste, sep = " / ")

fileNames <- full_list %>% map(.f = names) # uninteded outcome: provides names of all list, try to get the df names instead


# old_names <- old_col[names(test_files) %in% old_col]
# new_names <- new_col[names(test_files) %in% old_col]


# new_files %>% map(. %>% select(one_of(old_col))) # retain selected columns

# test_files <- copy(new_files[[1]])
# test_files
# ?setnames

# determine correct schema_mapping_finance_working_sheet.csv

mapped_schemas <- nm[names(nm) %in% names(new_files)]

new_files # new files
original_name <- mapped_schemas %>% map(. %>% select(ORIGINAL_NAME))%>% at_depth(1, unlist, use.names = FALSE)

# %>% flatten()
new_name <- mapped_schemas %>% map(. %>% select(Action)) %>% at_depth(1, unlist, use.names = FALSE)
# %>% flatten()
str(new_name)

new_name %>% at_depth(1, as.data.frame)
new_name %>% at_depth(1, unlist, use.names = FALSE)

# see if mapping is possible once
setnames(new_files[[5]],original_name[[5]][[2]], new_name[[5]][[36]])

# see if mapping is possible in a loop
setnames(x = asdf, old = asdf, new = sadf)

pmap(.l = list(x = new_files[13], old = original_name[13], new = new_name[13]), .f = setnames)
pmap(.l = c(x = new_files[5], old = list(original_name[5]), new = list(new_name[[5]])), .f = setnames)


# list of column headers to reconcile
# rec_list <- map(.x = fileList_xlsx[[1]], get_schema) %>% bind_rows() %>%
#   arrange(Name)
# 
# rec_list <- map(.x = fileList_flat, get_schema) %>% bind_rows() %>% 
#   arrange(Name)
# 
# rec_list <- map(.x = fileList_flat, get_schema_flat) %>% bind_rows() %>% 
#   arrange(Name) %>% 
#   group_by(Name) %>% 
#   summarise(n =n()) %>% 
#   filter(n <3) %>% 
#   distinct(Name, Name_Formatted, Name_Suggested, Source, Source_Long) %>% 
#   group_by(Name, Name_Formatted, Name_Suggested, Source, Source_Long)

#create file names for output this will match the csv_conversion value for schema_mapping_finance_working_sheet.csv

# this function will insert each worksheet into a list element regardless of worksheet name
# it returns a list that gets 'flattened' below
# depricated for now
# read_excel_allsheets <- function(filename, directory = getwd()) {
#   sheets <- readxl::excel_sheets(filename)
#   # source <- stringr::str_extract(pattern = "[^/]+$", string = filename)
#   source <- map(.x = filename, .f = ~stringr::str_extract(pattern = "[^/]+$", string = .x))
#   # x  <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
#   # x <- map2(.x = filename,.y = sheets, .f = readxl::read_xlsx, n_max = 1)
#   # x <- map2(.x = filename,.y = sheets, .f = readxl::read_excel, n_max = 1) %>% 
#   x <- map2(.x = filename,.y = sheets, .f = readxl::read_excel, n_max = 1) %>% 
#     map( ~gather(data = .x, key = variable_name, value = value)) %>% 
#     map2(.y = source,.f = ~mutate(.x, source = .y)) %>% 
#     map2(.y = sheets,.f = ~mutate(.x, sheet = .y)) %>% 
#     map(.f = bind_rows) %>% bind_rows()
#     # map(.f = filter(stringr::str_detect(pattern = "[0-9]", string = variable_name)==TRUE)) 
#   # x <- readxl::read_xlsx(path = filename, sheet = sheets, trim_ws = T, n_max = 2)
#   # x  <-    lapply(sheets, function(X) readxl::read_excel(fileList[[1]], sheet = X))
#   # x <- map(.x = fileList[[1]], .f = readxl::read_xlsx)
#   # x <- map(.x = filename, .f = readxl::read_xlsx, sheets)
#   # names(x) <- sheets
#   # return(x)
# }
# 
# read_one_sheet_dep <- function(filename, directory = getwd()) {
#   # source <- stringr::str_extract(pattern = "[^/]+$", string = filename)
#   source <- map(.x = filename, .f = ~stringr::str_extract(pattern = "[^/]+$", string = .x))
#   # x  <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
#   # x <- map2(.x = filename,.y = sheets, .f = readxl::read_xlsx, n_max = 1)
#   # x <- map2(.x = filename,.y = sheets, .f = readxl::read_excel, n_max = 1) %>% 
#   x <- map2(.x = filename,.y = sheets, .f = readxl::read_excel, n_max = 1) %>% 
#     map( ~gather(data = .x, key = variable_name, value = value)) %>% 
#     map2(.y = source,.f = ~mutate(.x, source = .y)) %>% 
#     map2(.y = sheets,.f = ~mutate(.x, sheet = .y)) %>% 
#     map(.f = bind_rows) %>% bind_rows()
#   # map(.f = filter(stringr::str_detect(pattern = "[0-9]", string = variable_name)==TRUE)) 
#   # x <- readxl::read_xlsx(path = filename, sheet = sheets, trim_ws = T, n_max = 2)
#   # x  <-    lapply(sheets, function(X) readxl::read_excel(fileList[[1]], sheet = X))
#   # x <- map(.x = fileList[[1]], .f = readxl::read_xlsx)
#   # x <- map(.x = filename, .f = readxl::read_xlsx, sheets)
#   # names(x) <- sheets
#   # return(x)
# }


# https://www.reddit.com/r/rstats/comments/60peaa/best_way_to_work_with_xlsx_files_with_multiple/


# get_schema <- function(x) {
#   source <- basename(x) %>% tools::file_path_sans_ext()
#   source_long <- basename(x) 
#   readxl::read_excel(x, n_max = 1, trim_ws = T) %>%
#     gather(key = name, value = Sample_Data) %>%
#     mutate( Name_Formatted = tolower(gsub(x = name, replacement = "_", pattern = " ")),
#             Name_Suggested = Name_Formatted,
#             Source = basename(x) %>% tools::file_path_sans_ext(),
#             Source_Long = basename(x),
#             Status = '',
#             Notes = '',
#             Product = '') %>%  
#     # mutate(Concat = trimws(paste0(.$name, .$Source))) %>% 
#     select(Name = name, Name_Formatted, Name_Suggested, Source, Source_Long)
# }


# get_schema_flat <- function(x) {
#   source <- basename(x) %>% tools::file_path_sans_ext()
#   source_long <- basename(x) 
#   data.table::fread(x, nrows = 1, strip.white = T) %>% as_data_frame() %>% 
#     gather(key = name, value = Sample_Data) %>%
#     mutate( Name_Formatted = tolower(gsub(x = name, replacement = "_", pattern = " ")),
#             Name_Suggested = Name_Formatted,
#             Source = basename(x) %>% tools::file_path_sans_ext(),
#             Source_Long = basename(x),
#             Status = '',
#             Notes = '',
#             Product = '') %>%  
#     # mutate(Concat = trimws(paste0(.$name, .$Source))) %>% 
#     select(Name = name, Name_Formatted, Name_Suggested, Source, Source_Long)
# }

# remove_pivots <- function(df, ...) {
#   select(df,-matches("X__"))
#   # mutate(Concat = trimws(paste0(.$name, .$Source))) %>%
# }