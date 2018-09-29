DROP TABLE IF EXISTS `rda`.`rdadata`.`usg_2017_hetoua_b`;
CREATE TABLE `rda`.`rdadata`.`usg_2017_hetoua_b`
(calendar_date,
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
    wea_data_typ_cd,
    baseline_terr_cd,
    opr_area_cd,
    usg_hr,
    rt_sched_cd,
    tou_cd, usg_amt)
PARITION BY (day_of_year)

(SELECT cust.model_id, cust.group_id,
    cust.uniq_sa_id,
    cust.deriv_baseline_terr_cd,
    cust.opr_area_cd,
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
    tou.rt_sched_cd,
    tou.tou_cd,
    usg.usg_amt,
    DATE_PART('hour', usg.elec_intvl_end_dttm) AS usg_hr
FROM `rda`.`rdadata`.`model_population_b` AS cust
INNER JOIN `rda`.`rdatables`.`elec_intvl_usg_all` AS usg
    ON cust.uniq_sa_id = usg.uniq_sa_id

    INNER JOIN  `rda`.`rdadata`.`time_shift_xref` AS xref
    ON usg.usg_dt = xref.calendar_date
where usg_id = D
JOIN  `rda`.`rdadata`.`tou_lookup_2017_hetoua` AS tou
    ON usg.usg_dt = tou.calendar_date
    AND usg.elec_intvl_end_dttm BETWEEN tou.tou_data_from_dttm AND tou.tou_data_to_dttm

