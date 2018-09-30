DROP TABLE IF EXISTS rda.rdadata.bill_2018_hetoua_q;
CREATE TABLE rda.rdadata.bill_2018_hetoua_q 
(model_id,
  group_id,
  uniq_sa_id,
  deriv_baseline_terr_cd,
  opr_area_cd,
  rt_sched_cd,
  tou_cd,
  day_of_year_shift,
  X,
  Y)
PARTITION BY (group_id)
AS
(
  SELECT
  usg_tou.model_id,
  usg_tou.group_id,
  usg_tou.uniq_sa_id,
  usg_tou.deriv_baseline_terr_cd,
  usg_tou.opr_area_cd,
  usg_tou.rt_sched_cd,
  usg_tou.tou_cd,
  usg_tou.day_of_year_shift,
  AVG(wea_tou.meas_val) AS X,
  SUM(usg_tou.usg_amt) AS Y
  FROM rda.rdadata.usg_2017_hetoua_q AS usg_tou
  JOIN rda.rdadata.wea_2017_hetoua AS wea_tou
  ON usg_tou.day_of_year = wea_tou.day_of_year
  AND usg_tou.train_year = wea_tou.train_year
  AND usg_tou.usg_hr = wea_tou.wea_hr
  AND usg_tou.opr_area_cd = wea_tou.opr_area_cd 
  AND usg_tou.rt_sched_cd = wea_tou.rt_sched_cd
  AND usg_tou.tou_cd = wea_tou.tou_cd
  AND wea_tou.wea_data_typ_id = 1
  GROUP BY
  usg_tou.model_id,
  usg_tou.group_id,
  usg_tou.uniq_sa_id,
  usg_tou.deriv_baseline_terr_cd,
  usg_tou.opr_area_cd,
  usg_tou.rt_sched_cd,
  usg_tou.tou_cd,
  usg_tou.day_of_year_shift
);
