d_list <- flatten_chr(map2('E:/CEA_DATA/interval_data/electric/kamal/2013_2015_interval_pivot_'
          , list('t_2.txt','w_2.txt','x_2.txt'), paste0))

rm(d_full)
d <- map(.x = d_list[3], .f = fread, header = TRUE, sep = 'auto'
          , check.names = TRUE
          , stringsAsFactors = TRUE
          , integer64 = 'character'
          , verbose = FALSE)

d <- rbindlist(d)



setkey(d, usg_dt)
d <- d[ener_dir_cd == 'D']
d[, date :=as.IDate(usg_dt, '%m/%d/%Y')]
d[, year := year(date)]
d <- d[year!=2013,]
d_full <- copy(d)
sample_sp_id <- d[,unique(sp_id)]
d_full <- d_full[sp_id %in% sample_sp_id]
rm(d)

full_obs <- as.character(d_full[,.N, by = .(sp_id)][N==730][,(sp_id)])
d_full <- subset(d_full, sp_id %in% c(full_obs))
setkey(d_full, sp_id, date)

HOURS_OLD <- c('kwh_0100', 'kwh_0200', 'kwh_0300', 'kwh_0400', 'kwh_0500', 
                                            'kwh_0600', 'kwh_0700', 'kwh_0800', 'kwh_0900', 'kwh_1000', 'kwh_1100', 
                                            'kwh_1200', 'kwh_1300', 'kwh_1400', 'kwh_1500', 'kwh_1600', 'kwh_1700', 
                                            'kwh_1800', 'kwh_1900', 'kwh_2000', 'kwh_2100', 'kwh_2200', 'kwh_2300', 
                                            'kwh_2400')

HOURS_NEW <- paste0(seq(0, 23))

setnames(d_full, old = HOURS_OLD, new = HOURS_NEW)

# Transform from wide to long
d_full[,month:=month.abb[data.table::month(date)]]
d_full <- melt.data.table(d_full, id.vars = c('sp_id', 'month', 'date')
               , measure.vars = HOURS_NEW, variable.name = 'hour', value.name = 'usage')
setkey(d_full, date)
d_full[, usage := as.numeric(usage)]
d_full[is.na(usage),usage:=0]
setcolorder(d_full, c('sp_id',	'date',	'hour',	'usage',	'month'))

# d_full[, `:=`(season = factor(ifelse(as.numeric(match(month, month.abb)) %in% c(4:10),'summer', 'winter'))
#          , weekend = wday(date) %in% c(1, 7)
#          , year = year(date)
#          , holiday = FALSE)]

OUTPUT_FULL <- 'E:/CEA_DATA/interval_data/electric/hourly_by_sp_id/inteval_14_15/'
d_full[,write_index:=sp_id]
gc()
d_full[, write.csv(.SD, paste0(OUTPUT_FULL,write_index,'.csv'), row.names = FALSE)
            , by = .(write_index)]



