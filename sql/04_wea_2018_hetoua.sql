DROP TABLE IF EXISTS `rda`.`rdadata`.`wea_2018_hetoua`;
CREATE TABLE `rda`.`rdadata`.`wea_2018_hetoua` 
(
  calendar_date,
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
  wea_stn_cd,
  wea_hr,
  wea_data_typ_cd,
  
  rt_sched_cd,
  tou_cd,
  meas_val
)

PARTITION BY (day_of_year)

AS
(
  SELECT 
  wea.calendar_date,
  wea.month_of_year,
  wea.day_of_month,
  wea.train_year,
  wea.predict_year,
  wea.day_of_week,
  wea.week_of_year,
  wea.quarter_of_year,
  wea.day_of_year,
  wea.day_of_year_shift, 
  
  wea.wea_data_typ_id,
  wea.wea_stn_cd,
  wea.wea_hr,
  wea.wea_data_typ_cd,
  
  tou.rt_sched_cd,
  tou.tou_cd,
  AVG(wea.meas_val) AS meas_val
  FROM
  (
    SELECT
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
    
    wea.wea_data_typ_id,
    wea.wea_stn_cd,
    wea.wea_dt,
    wea.wea_dttm, 
    DATE_PART('hour', wea.wea_dttm) AS wea_hr,
    
    CAST(wea.meas_val AS FLOAT) meas_val,
    typ.wea_data_typ_cd,
    
    terr_xref.baseline_terr_cd, 
    area_xref.opr_area_cd
    
    FROM `rda`.`rdadata`.`time_shift_xref` AS xref
    JOIN `rda`.`rdatables`.`weather_data`  AS wea
    ON xref.calendar_date = wea.wea_dt
    JOIN `rda`.`rdatables`.`weather_station` AS stn
    ON wea.wea_stn_cd = stn.wea_stn_cd 
    AND stn.wea_stn_owner = 'PGE'
    JOIN `rda`.`rdatables`.`weather_data_type` AS typ
    ON wea.wea_data_typ_id = typ.wea_data_typ_id
    AND wea.wea_data_typ_id IN (1, 22, 23) 
    
    INNER JOIN `rda`.`rdatables`.`weather_baseline_terr_xref` AS terr_xref
    ON wea.wea_stn_cd = terr_xref.wea_stn_cd 
    AND xref.month_of_year = terr_xref.mo_id
    
    JOIN `rda`.`rdatables`.`weather_op_area_xref` AS area_xref	
    ON terr_xref.wea_stn_cd = area_xref.wea_stn_cd
    
    WHERE xref.train_year = 2017
  ) AS wea
  JOIN  `rda`.`rdadata`.`tou_lookup_2017_hetoua` AS tou
  ON wea.calendar_date = tou.calendar_date
  AND wea.wea_dttm BETWEEN tou.tou_data_from_dttm AND tou.tou_data_to_dttm
  
  GROUP BY
  wea.calendar_date,
  wea.month_of_year,
  wea.day_of_month,
  wea.train_year,
  wea.predict_year,
  wea.day_of_week,
  wea.week_of_year,
  wea.quarter_of_year,
  wea.day_of_year,
  wea.day_of_year_shift, 
  
  wea.wea_data_typ_id,
  wea.wea_stn_cd,
  wea.wea_hr,
  wea.wea_data_typ_cd,
  tou.rt_sched_cd,
  tou.tou_cd
);
