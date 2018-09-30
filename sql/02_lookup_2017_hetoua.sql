DROP TABLE IF EXISTS  `rda`.`rdadata`.`tou_lookup_2017_hetoua`;
CREATE TABLE  `rda`.`rdadata`.`tou_lookup_2017_hetoua`
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
n_shift,
day_of_year_shift,
tou_map_dt,
tou_data_from_dttm,
tou_data_to_dttm,
tou_cd,
rt_sched_cd,
tou_map_typ_cd
)
PARTITION BY (day_of_year)
AS
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
xref.n_shift,
xref.day_of_year_shift,
tou_data.tou_map_dt,
tou_data.tou_data_from_dttm,
tou_data.tou_data_to_dttm,
tou_data.tou_cd,
tou_map.rt_sched_cd,
tou_map.tou_map_typ_cd
FROM `rda`.`rdatables`.`tou_map_data` AS tou_data
JOIN `rda`.`rdatables`.`rt_sched_tou_map` AS tou_map
ON tou_data.tou_map_id = tou_map.tou_map_id
JOIN  `rda`.`rdadata`.`time_shift_xref` AS xref
ON tou_data.tou_map_dt = xref.calendar_date
WHERE tou_map.rt_sched_cd IN
(
'HETOUA'
)
/*AND xref.train_year = 2017*/
)
