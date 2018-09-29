JOIN `rda`.`rdadata`.`tou_lookup_2017_hetoua` AS tou
ON wea.calendar_date = tou.calendar_date
AND wea.wea_dttm BETWEEN tou.tou_data_from_dttm AND tou.tou_data_to_dttm