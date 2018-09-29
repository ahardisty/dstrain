DROP TABLE IF EXISTS `rda`.`rdadata`.`time_shift_xref`
CREATE TABLE  `rda`.`rdadata`.`time_shift_xref`
(
  calendar_date,
  month_of_year,
  day_of_month,
  year_of_calendar,
  year_of_calendar_shift,
  day_of_week,
  week_of_year,
  quarter_of_year,
  day_of_year,
  n_shift,
  day_of_year_shift
)
PARTITION BY (year_of_calendar)
AS
(
  SELECT
  f.calendar_date,
  f.month_of_year,
  f.day_of_month,
  d.year_of_calendar,
  d.year_of_calendar_shift,

  f.day_of_week,
  f.week_of_year,
  f.quarter_of_year,
  f.day_of_year,
  d.n_shift,
  CASE WHEN f.day_of_year - d.n_shift > day_of_year_max THEN (f.day_of_year - d.n_shift) - day_of_year_max
  ELSE f.day_of_year - d.n_shift
  END day_of_year_shift
  FROM
  (
    SELECT
    n_shift,
    year_of_calendar,
    year_of_calendar_shift
    FROM
    (
      SELECT
      a.calendar_date,
      a.day_of_week,
      a.year_of_calendar,
      b.day_of_week AS day_of_week_shift,
      b.year_of_calendar AS year_of_calendar_shift
      , CASE WHEN (a.day_of_week  - b.day_of_week ) > 0 THEN a.day_of_week  - b.day_of_week - 7
      ELSE a.day_of_week  - b.day_of_week
      END n_shift
      FROM `rda`.`rdatables`.`sys_calendar` AS a
      LEFT JOIN `rda`.`rdatables`.`sys_calendar` AS b
      ON a.day_of_year = b.day_of_year
      WHERE a.year_of_calendar IN ('2017','2018') AND b.year_of_calendar IN ('2018','2019')
      AND a.day_of_year BETWEEN 1 AND 7 AND b.day_of_year BETWEEN 1 AND 7
    )
    GROUP BY
    n_shift,
    year_of_calendar,
    year_of_calendar_shift
  ) AS d

  JOIN
  (
    SELECT
    calendar_date,
    day_of_week,
    day_of_month,
    day_of_year,
    week_of_year,
    month_of_year,
    quarter_of_year,
    year_of_calendar,
    MAX(day_of_year) OVER (PARTITION BY year_of_calendar) AS day_of_year_max
    FROM `rda`.`rdatables`.`sys_calendar`
  ) AS f
  ON d.year_of_calendar = f.year_of_calendar
)
