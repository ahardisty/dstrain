DROP TABLE IF EXISTS `rda`.`rdadata`.`summary_population`;

CREATE TABLE `rda`.`rdadata`.`summary_population`
AS (SELECT cust.deriv_baseline_terr_cd, cust.opr_area_cd,
COUNT(DISTINCT cust.uniq_sa_id) AS cnt_uniq_sa_id
FROM `rda`.`rdatables`.`cust_yearly` AS cust

WHERE sa_sta_cd = 20
    AND snapshot_typ = 'ROLLING'
    AND res_ind = 'Y'
    AND  net_mtr_ind = 'N'
    AND cmprs_rt_sched_cd IN
    ('E1'
      ,'E1L'
      ,'E1'
      ,'E1L'
      ,'E6'
      ,'ETOUA'
      ,'ETOUB'
      ,'E6L'
      ,'ETOUAL'
      ,'ETOUP1L'
      ,'ETOUP1'
      ,'ETOUP2'
      ,'ETOUP3'
      ,'ETOUP2L'
      ,'ETOUP3L'
      ,'ETOUBL'
      ,'E6'
      ,'E6L'
      ,'ETOUB'
      ,'ETOUA'
      ,'ETOUAL'
      ,'ETOUBL'
    )
    AND deriv_rt_sched_cd NOT LIKE
    (
      'SE%'
    )
GROUP BY cust.deriv_baseline_terr_cd, cust.opr_area_cd );

DROP VIEW IF EXISTS `rda`.`rdatables`.`summary_population`;
CREATE VIEW `rda`.`rdatables`.`summary_population`
AS (SELECT cust.deriv_baseline_terr_cd, cust.opr_area_cd, cnt_uniq_sa_id
FROM `rda`.`rdadata`.`summary_population` AS cust)
