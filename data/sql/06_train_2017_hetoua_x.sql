DROP TABLE IF EXISTS `rda`.`rdadata`.train_2017_hetoua_x;
CREATE TABLE `rda`.`rdadata`.train_2017_hetoua_x
(group_id,
    model_id,
    uniq_sa_id,
    baseline_terr_cd,
    opr_area_cd,
    train_year,
    predict_year,
    rt_sched_cd,
    tou_cd,
    wea_data_typ_id,
    day_of_year_shift,
    X,
    Y)
PARITION BY (group_id)
AS (SELECT
    cust_usg.group_id,
    cust_usg.model_id,
    cust_usg.uniq_sa_id,
    cust_usg.deriv_baseline_terr_cd,
    cust_usg.opr_area_cd,
    cust_usg.train_year,
    cust_usg.predict_year,
    cust_usg.day_of_year,
    cust_usg.day_of_year_shift,
    cust_usg.rt_sched_cd,
    cust_usg.tou_cd,
    AVG(wea_tou.meas_val) AS X,
    SUM(cust_usg.usg_amt) AS Y
    FROM
(SELECT
    cust_usg.group_id,
    cust_usg.model_id,
    cust_usg.uniq_sa_id,
    cust_usg.deriv_baseline_terr_cd,
    cust_usg.opr_area_cd,
    cust_usg.calendar_date,
    cust_usg.month_of_year,
    cust_usg.day_of_month,
    cust_usg.train_year,
    cust_usg.predict_year,
    cust_usg.day_of_week,
    cust_usg.week_of_year,
    cust_usg.quarter_of_year,
    cust_usg.day_of_year,
    cust_usg.day_of_year_shift,
    cust_usg.rt_sched_cd,
    cust_usg.tou_cd,
    cust_usg.usg_amt,
    cust_usg.usg_hr,
    wea_tou.wea_hr,
    wea_tou.meas_val)
FROM `rda`.`rdadata`.`usg_2017_hetoua_x` AS cust_usg
INNER JOIN `rda`.`rdadata`.`wea_2017_hetoua` AS wea_tou
    ON cust_usg.day_of_year = wea_tou.day_of_year
    AND cust_usg.usg_hr = wea_tou.wea_hr
    AND cust_usg.rt_sched_cd = wea_tou.rt_sched_cd
    AND cust_usg.tou_cd = wea_tou.tou_cd


GROUP BY
    group_id,
    model_id,
    uniq_sa_id,
    deriv_baseline_terr_cd,
    opr_area_cd,
    train_year,
    predict_year,
    day_of_year,
    day_of_year_shift,
    rt_sched_cd,
    tou_cd
);
DROP VIEW IF EXISTS `rda`.`rdatables`.train_2017_hetoua_x;
CREATE VIEW `rda`.`rdatables`.train_2017_hetoua_x
AS(SELECT * FROM `rda`.`rdadata`.train_2017_hetoua_x);
